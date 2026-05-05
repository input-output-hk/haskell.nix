{ pkgs, buildPackages, stdenv, lib, haskellLib, ghc, compiler-nix-name, fetchurl, runCommand, comp-builder, comp-v2-builder, setup-builder, builderVersion ? 1 }:

config:
{ flags
, package
, components
, cabal-generator

, name
, sha256
, src
, package-description-override
, patches

, shellHook

, ...
}@pkg:

let
  # Some packages bundled with GHC are not the same as they are in hackage.
  bundledSrc = {
      # These are problematic because the hackage versions will not install and are part of LTS.
      "ghc902/stm-2.5.0.0" = "/libraries/stm";
      "ghc902/filepath-1.4.2.1" = "/libraries/filepath";
    }."${compiler-nix-name}/${name}" or null;
  src =
    if bundledSrc != null
      then ghc.configured-src + bundledSrc
    else pkg.src;

  cabalFile = if package-description-override == null || bundledSrc != null then null else package-description-override;

  # New GHC JS backend run emcc itself without the need for custom Setup.hs
  oldGhcjs = stdenv.hostPlatform.isGhcjs && builtins.compareVersions ghc.version "9.10" < 0;
  defaultSetupSrc = if oldGhcjs then ./Setup.ghcjs.hs else ./Setup.hs;

  setup = if package.buildType == "Simple"
    then
      if oldGhcjs
        then
          buildPackages.haskell-nix.nix-tools-unchecked.exes.default-setup-ghcjs // { exeName = "default-setup-ghcjs"; }
        else
          buildPackages.haskell-nix.nix-tools-unchecked.exes.default-setup // { exeName = "default-setup"; }
    else setup-builder ({
      component = components.setup // {
        depends = config.setup-depends ++ components.setup.depends ++ package.setup-depends;
        extraSrcFiles = components.setup.extraSrcFiles ++ [ "Setup.hs" "Setup.lhs" ];
        pkgconfig = if components ? library then components.library.pkgconfig or [] else [];
      };
      inherit package name src flags patches defaultSetupSrc;
      inherit (pkg) preUnpack postUnpack prePatch postPatch;
    } // lib.optionalAttrs (package.buildType != "Custom") {
      nonReinstallablePkgs = ["base" "Cabal"];
    });

  buildComp = allComponent: componentId: component:
    let v1 = comp-builder {
          inherit allComponent componentId component package name src flags setup cabalFile cabal-generator patches
                  shellHook
                  ;
          inherit (config) prebuilt-depends;
        };
        v2 = comp-v2-builder {
          inherit componentId component package name src flags patches cabalFile;
          inherit (pkg) prePatch postPatch;
          # Compute the set of (pkg-name, pkg-version) pairs that
          # cabal's solver needs to plan for when we target this
          # pkg via the shim's `build-depends`.  We stay inside
          # plan-json (pure JSON data) to avoid forcing module
          # options that might reach back into our own construction
          # — iterating `config.packages.<id>.allComponent` from
          # here caused infinite recursion through haskell.nix's
          # internal cross-refs.
          #
          # For every install-plan entry with the same pkg-name +
          # pkg-version as this instance, take its `depends` and
          # `exe-depends` (each is a list of plan ids).  Resolve
          # each id back to its canonical pkg-name + pkg-version
          # via the same plan-json.  Drop same-pkg self-refs.
          homeDependIds =
            let
              plan     = config.plan-json.install-plan;
              byId     = config.plan-json-by-id;
              sibs     = lib.filter
                (p: (p.pkg-name or null) == package.identifier.name
                    && (p.pkg-version or null) == package.identifier.version)
                plan;
              # Gather every `depends` AND `exe-depends` entry
              # from sibling plan entries.  plan-json's layout
              # varies by build-type:
              #   * Simple pkgs get one entry per component, each
              #     with top-level `depends` / `exe-depends`.
              #   * Custom pkgs get a single entry with a
              #     `components` sub-object (keys like `lib`,
              #     `exe:name`, `setup`), each of which has its own
              #     `depends` / `exe-depends`.
              # We include `exe-depends` too because cabal's
              # re-solver inside each slice needs build-tool
              # packages (hsc2hs, alex, happy, ...) to be resolvable
              # the same way plan-nix's solver resolved them.
              depsFromEntry = p:
                (p.depends or [])
                ++ (p.exe-depends or [])
                ++ lib.concatMap
                     (c: (c.depends or []) ++ (c.exe-depends or []))
                     (lib.attrValues (p.components or {}));
              depIds   = lib.unique (lib.concatMap depsFromEntry sibs);
              idToNV = id:
                let q = byId.${id} or null;
                in if q == null
                   then null
                   else { name = q.pkg-name; version = q.pkg-version; };
              # Boot packages (base, ghc-prim, rts, ...) don't have
              # slicable haskell.nix builds — they come with GHC and
              # are already registered in the starting package db.
              # `pre-existing` in plan-json flags these; skip them.
              preExisting = lib.filter
                (p: (p.type or null) == "pre-existing")
                plan;
              preExistingNames = map (p: p.pkg-name) preExisting;
              isExcluded = nv:
                nv == null
                || nv.name == package.identifier.name
                || lib.elem nv.name preExistingNames;
            in lib.filter (nv: !(isExcluded nv)) (map idToNV depIds);
          pkgSet = config.packages;
          packageIdsByName = config.package-ids-by-name;
          # Raw plan.json `install-plan` array — comp-v2-builder
          # walks the lib-dep closure (via `depends`, not following
          # `exe-depends`) to pin each transitive lib dep's
          # version on the shim's `build-depends` line.
          planJson = config.plan-json.install-plan;
          # Pre-built `id -> entry` index over `planJson`, computed
          # once in `modules/install-plan/override-package-by-name.nix`
          # and shared across all slices so the v2 builder doesn't
          # rebuild the same N-entry attrset per component.
          planJsonByPlanId = config.plan-json-by-id;
          # Whether this package references one of its own exes
          # via `exe-depends` (the resolved `build-tool-depends` /
          # `build-tools`) anywhere in its plan entries (lib, exe,
          # test-suite, ...).  A `True` here means the package's
          # own build process needs the exe on PATH at build time
          # — and for cross-compile that exe must be the
          # build-platform version, not the just-built host one.
          # alex's test-suite has `build-tools: alex` and triggers
          # this; just-template-haskell doesn't.
          packageRefersOwnExe =
            let
              plan  = config.plan-json.install-plan;
              byId  = config.plan-json-by-id;
              sibs  = lib.filter
                (p: (p.pkg-name or null) == package.identifier.name)
                plan;
              exeDepsOf = p:
                (p.exe-depends or [])
                ++ lib.concatMap (c: c.exe-depends or [])
                     (lib.attrValues (p.components or {}));
              isSelfId = id:
                let q = byId.${id} or null;
                in q != null && q.pkg-name == package.identifier.name;
            in lib.any (p: lib.any isSelfId (exeDepsOf p)) sibs;
        };
    # `builderVersion` (propagated from the project-level option)
    # picks the derivation at `hsPkgs.<pkg>.components.<kind>.<n>`.
    # Switch builders by setting the project-level `builderVersion`
    # option (1 or 2) — there's no per-component opt-in / opt-out.
    in if builderVersion == 2 then v2 else v1;

in rec {
  components = haskellLib.applyComponents (buildComp pkg.allComponent) pkg;
  checks = pkgs.lib.recurseIntoAttrs (builtins.mapAttrs
    (_: d: haskellLib.check d)
      (lib.filterAttrs (_: d: d.config.doCheck) components.tests));
  inherit (package) identifier detailLevel isLocal isProject buildType;
  inherit setup;
  isHaskell = true;
  inherit src;
}
