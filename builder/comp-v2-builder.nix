# v2 component builder.  Built once per component when the project's
# `builderVersion = 2`; `hspkg-builder.nix` then exposes the result
# at `hsPkgs.<pkg>.components.<kind>.<n>` as the primary derivation.
# Set `builderVersion = 1` on the project module to use the legacy
# Setup.hs-based comp-builder.nix instead.
#
# Compared to the overlay (test/cabal-store-compose/phase5/add-v2.nix)
# this lives inside the module system's evaluation so it reads
# ghc-options / configure-options / flags / patches / libs / build-tools
# directly from config.packages rather than via a project wrapper.
#
# Scope matches the overlay: library, executable, test-suite, and
# benchmark components.  See the overlay for the longer explanation
# of why we pick each cabal.project shape, target selector, and
# binary-extraction path per kind.
{ lib, stdenv, runCommand, buildCabalStoreSlice, composeStore, ghc, hsPkgs, pkgs, haskellLib
# Optional cross-TH plumbing for the host platform.  Currently
# only set by overlays/windows.nix and exposes `wrapGhc :: ghc -> ghc`.
, templateHaskell ? null }:

{ componentId, component, package, name, src, flags ? {}, patches ? [], cabalFile ? null, pkgSet
, cabal-generator ? null # "hpack" if the package ships only `package.yaml`
                         # and needs hpack run to produce the .cabal file
                         # before tarball / build.  Mirrors v1's
                         # comp-builder.nix handling.
, packageIdsByName ? {}  # { <canonical-pkg-name> = [<plan-id>...]; }
, planJson ? []          # plan.json's `install-plan` — used to walk
                         # the transitive lib-dep closure for the
                         # cabal.project `constraints:` block.
, planJsonByPlanId ? {}  # `planJson` indexed by `id` — pre-built once
                         # at the project level, shared across slices.
, homeDependIds    ? []  # [{ name; version; }] — sibling-component LIB deps from plan-json
, homeBuildToolIds ? []  # [{ name; version; }] — sibling-component EXE deps from plan-json
, prePatch ? null, postPatch ? null, ... }@allArgs:

let
  pkgName = package.identifier.name;
  pkgVersion = package.identifier.version;
  ghcPkgVersion = ghc.version;

  # Cross-compile detection — drives:
  #   * whether home-build-tool source goes in the slicing repo
  #     (no on cross — see `depTransitives` and `externalDepIds`).
  #   * whether the slice's cabal v2-build gets explicit
  #     `--with-PROG=PATH` flags for transitive build-tool exes
  #     (yes on cross — see `withProgFlags`).
  #   * whether the slice runs the unit-id mismatch check
  #     (no on cross — `expectedUnitId` returns null; cross plan
  #     unit-ids legitimately diverge from what cabal computes
  #     against the real cross GHC).
  isCross = (ghc.targetPrefix or "") != "";

  # Plan-nix entry for this slice's own unit (looked up via the
  # unit-id `modules/install-plan/planned.nix` recorded on the
  # `package.identifier`).  We use it to detect source-repo packages
  # (`pkg-src.type == "source-repo"`) so the slice's cabal.project
  # can use the same source-repository-package shape the project's
  # plan-nix was built from.  Reproducing the project's
  # `cabal.project` shape is what makes the slice's `pkg-src-sha256`
  # and therefore the slice's UnitId match plan-nix.
  thisPlanEntry =
    let uid = package.identifier.unit-id or null;
    in if uid == null then null else planJsonByPlanId.${uid} or null;
  isSourceRepoPkg =
    thisPlanEntry != null
    && (thisPlanEntry.pkg-src or {}).type or null == "source-repo";
  # haskell.nix wraps the fetched source-repo content in a minimal
  # git repo at project-eval time (see
  # `lib/call-cabal-project-to-nix.nix:209` — `git init -b minimal &&
  # git add . && git commit`).  We rebuild that wrapper here from
  # the *fetched source* (`${src}` for source-repo packages, which
  # is the project-level `fetched` derivation).  cabal hashes the
  # cloned source content (not the wrapper repo's git hash), so a
  # locally-rebuilt wrapper still produces the same
  # `pkg-src-sha256`.
  minimalSourceRepo = pkgs.runCommand "v2-source-repo-${pkgName}-${pkgVersion}"
    { nativeBuildInputs = [ pkgs.rsync pkgs.gitMinimal ];
      preferLocalBuild = true;
    }
    ''
      mkdir $out
      rsync -a --prune-empty-dirs --chmod=u+w "${src}/" "$out/"
      cd $out
      git init -b minimal
      git add --force .
      GIT_COMMITTER_NAME='No One' GIT_COMMITTER_EMAIL= \
        git commit -m "Minimal Repo For Haskell.Nix" --author 'No One <>'
    '';

  # Helpers mirroring add-v2.nix ----------------------------------

  # `pkgSet` is keyed by both canonical package names (`foo`) and
  # the plan-id-keyed entries the install plan generates per
  # configuration (`foo-0.1.0.0-inplace`,
  # `foo-0.1.0.0-inplace-foo-exe`, ...).  Per-plan-unit settings
  # sourced from plan.json's `configure-args` (see
  # modules/install-plan/configure-args.nix) land on the plan-id
  # entries.  Use `package-ids-by-name` (from
  # modules/install-plan/override-package-by-name.nix) to walk
  # every plan id for a given canonical name.
  cfgsForCanonical = pname:
    map (id: pkgSet.${id})
      (lib.filter (id: pkgSet ? ${id})
        (packageIdsByName.${pname} or [ pname ]));

  componentEntries = cfg:
    let comps = cfg.components or {};
    in lib.optional (comps ? library) { name = "library"; comp = comps.library; }
       ++ lib.mapAttrsToList (n: c: { name = "sublib:${n}"; comp = c; }) (comps.sublibs or {})
       ++ lib.mapAttrsToList (n: c: { name = "exe:${n}";    comp = c; }) (comps.exes or {})
       ++ lib.mapAttrsToList (n: c: { name = "test:${n}";   comp = c; }) (comps.tests or {})
       ++ lib.mapAttrsToList (n: c: { name = "bench:${n}";  comp = c; }) (comps.benchmarks or {});

  # Plan-json flags for the matching source-repo unit, when this
  # package is built from a `source-repository-package`.  haskell.nix's
  # module-system flag overrides (e.g. `packages.<pkg>.flags.release =
  # true`) are applied per-component AFTER plan computation, so they
  # don't reach cabal-install when it computes the project plan — but
  # they *would* reach cabal-install when the slice rebuilds via the
  # same `source-repository-package` block.  Honouring them in the
  # slice would compute a different `pkgHashFlagAssignment` than
  # plan-nix and fork the UnitId.  For source-repo packages we
  # therefore use plan-json's flags as the source of truth,
  # reproducing the assignment cabal-install actually saw at plan
  # time.  Hackage / repo-tar / local packages keep the module-flag
  # behaviour (matches what they got at plan time too).
  planFlagsFor = pname:
    let unit = lib.findFirst
                 (p: (p.pkg-name or null) == pname)
                 null planJson;
        srcType = if unit == null then null
                  else (unit.pkg-src or {}).type or null;
    in if unit != null && srcType == "source-repo"
       then unit.flags or {}
       else null;
  flagBlockFor = pname:
    let
      moduleFlags = lib.foldl' (acc: cfg: acc // (cfg.flags or {})) {} (cfgsForCanonical pname);
      planFlags = planFlagsFor pname;
      flagAttrs = if planFlags != null then planFlags else moduleFlags;
      toToken = n: v: (if v then "+" else "-") + n;
      flagTokens = lib.mapAttrsToList toToken flagAttrs;
    in if flagTokens == []
       then ""
       else "package ${pname}\n  flags: ${lib.concatStringsSep " " flagTokens}\n";

  # Collect values for `field` across every cfg (canonical +
  # plan-id-keyed) for the package — at both the package level
  # (where `configure-args.nix` sets per-plan-unit overrides) and
  # the component level (where user-set values via `modules` land).
  # Errors if components or plan units disagree — cabal.project's
  # `package X\n  <field>:` stanza applies to all components, so we
  # can't faithfully reproduce per-component differentiation here.
  perPackageOptionOf = field: pname:
    let
      cfgs = cfgsForCanonical pname;
      pkgLevel = lib.imap0
        (i: cfg: { name = "pkg-${toString i}"; opts = cfg.${field} or []; })
        cfgs;
      compLevel = lib.concatMap (cfg:
        map (e: { name = e.name; opts = e.comp.${field} or []; })
          (componentEntries cfg)
      ) cfgs;
      tagged  = pkgLevel ++ compLevel;
      nonEmpty = lib.filter (e: e.opts != []) tagged;
      distinctSets = lib.unique (map (e: e.opts) nonEmpty);
    in
      if lib.length distinctSets <= 1
      then (if nonEmpty == [] then [] else (builtins.head nonEmpty).opts)
      else throw (
        "comp-v2-builder: package ${pname} has differing ${field} "
        + "across components / plan units:\n"
        + lib.concatMapStrings (e:
            "  " + e.name + ": " + builtins.toJSON e.opts + "\n"
          ) nonEmpty
        + "cabal.project's `package ${pname}` stanza applies to all "
        + "components; v2 cannot faithfully reproduce per-component "
        + "values.  Unify them or fall back to the v1 builder."
      );

  ghcOptionsOf       = perPackageOptionOf "ghcOptions";
  configureOptionsOf = perPackageOptionOf "configureOptions";

  # Has plan-nix flagged this canonical package (any of its units)
  # as documentation-enabled?  Mirrors `docEnabled` above but for
  # arbitrary pkg-names rather than this slice's own unit.
  pkgDocEnabled = pname:
    lib.any
      (e: (e.pkg-name or "") == pname
          && lib.elem "--ghc-option=-haddock" (e.configure-args or []))
      planJson;

  # `documentation: True` widens `pkgHashConfigInputs` (sets the
  # haddock-* booleans on ElaboratedConfiguredPackage) AND adds
  # `--ghc-option=-haddock` to the unit's configure-args.  When
  # we emit a `documentation: True` block here we therefore must
  # also strip `-haddock` from the per-pkg ghc-options block —
  # otherwise the slice would get `-haddock` twice in
  # `pkgHashGhcOptions` and the UnitId would diverge from plan-nix.
  ghcOptionsBlockFor = pname:
    let raw = ghcOptionsOf pname;
        opts = if pkgDocEnabled pname
               then lib.filter (o: o != "-haddock") raw
               else raw;
    in if opts == [] then ""
       else "package ${pname}\n  ghc-options: ${lib.concatStringsSep " " opts}\n";

  # Per-package `documentation: True` block.  Emitted when this
  # package's plan-nix entry has `--ghc-option=-haddock` (the
  # signal that `documentation: True` was set in the project's
  # cabal.project).  See `docEnabled` above for the full
  # rationale.
  documentationBlockFor = pname:
    if pkgDocEnabled pname
      then "package ${pname}\n  documentation: True\n"
      else "";

  configureOptionsBlockFor = pname:
    let opts = configureOptionsOf pname;
    in if opts == [] then ""
       else "package ${pname}\n  configure-options: ${lib.concatStringsSep " " opts}\n";

  # Restrict `package <name>` blocks to packages this slice
  # actually deals with — the target package itself plus the
  # transitive deps already shipped via the slicing repo.
  # Including unrelated packages would let their settings leak
  # into this slice's cabal.project hash and cause spurious
  # rebuilds when a sibling package changes.  Sort by name so
  # the emitted cabal.project — and the slice's drv hash — is
  # stable when input ordering shifts.
  sliceCanonicalNames =
    # `attrNames` on a `genAttrs`-built map dedupes the string list
    # in O(n log n) and returns a sorted result — `lib.unique` would
    # fall back to a quadratic scan, and the names are plain strings
    # so `haskellLib.uniqueWithName` doesn't apply.
    builtins.attrNames
      (lib.genAttrs
        ([ pkgName ] ++ map (e: e.name) (lib.attrValues depTarballsDeduped))
        (_: null));

  allFlagBlocks             = lib.concatMapStrings flagBlockFor             sliceCanonicalNames;
  allGhcOptionsBlocks       = lib.concatMapStrings ghcOptionsBlockFor       sliceCanonicalNames;
  allConfigureOptionsBlocks = lib.concatMapStrings configureOptionsBlockFor sliceCanonicalNames;
  allDocBlocks              = lib.concatMapStrings documentationBlockFor    sliceCanonicalNames;

  # Resolve version-conditional patches (a `patches` entry may be a
  # function `{ version }: drv-or-null`, same convention v1 uses).
  resolvedPatches = lib.filter (p: p != null) (map (p:
    if builtins.isFunction p
      then p { inherit (package.identifier) version; }
      else p
  ) patches);
  # Honour `prePatch`/`postPatch` (pkg-level hooks) around patching.
  # Hadrian, for example, uses `prePatch = "cd .."` to apply GHC-repo-
  # relative patches from outside the hadrian/ subdir.
  prePatchHook  = lib.optionalString (prePatch  != null) prePatch;
  postPatchHook = lib.optionalString (postPatch != null) postPatch;
  # Package tarball with patches applied.  Runs the pre/post hooks
  # inside the staged source dir so relative `cd ..` tricks work.
  patchApplyScript = lib.optionalString
    (resolvedPatches != [] || prePatch != null || postPatch != null) ''
    chmod -R u+w $target
    ( cd $target
      ${prePatchHook}
      ${lib.concatMapStrings (p: ''
        echo "Applying patch: ${builtins.baseNameOf (toString p)}" >&2
        patch -p1 < ${p}
      '') resolvedPatches}
      ${postPatchHook}
    )
  '';
  cabalFileOverride = lib.optionalString (cabalFile != null) ''
    chmod -R u+w $target
    cp ${builtins.toFile "${pkgName}.cabal" cabalFile} $target/${pkgName}.cabal
  '';
  # Local packages with `cabal-generator: hpack` ship `package.yaml`
  # without a generated `.cabal`.  Mirror v1's comp-builder.nix:
  # run hpack inside the package source dir so the tarball carries
  # a real `.cabal` for cabal-install to extract.  Skip when an
  # explicit `cabalFile` override is set (e.g. an X-revision from
  # `package-description-override`) since that already provides the
  # .cabal we want.
  hpackScript = lib.optionalString
    (cabalFile == null && cabal-generator == "hpack" && package.isLocal) ''
    chmod -R u+w $target
    ( cd $target
      ${pkgs.buildPackages.haskell-nix.nix-tools-unchecked}/bin/hpack
    )
  '';
  # When `prePatch`/`postPatch` hooks are defined for a local package
  # we stage the parent directory as well, so hooks like
  # `prePatch = "cd .."` (hadrian) can reach sibling files.  The
  # final tarball still contains only the package's own subdir.
  pkgHasHook = prePatch != null || postPatch != null;
  # `cabalFile` (X-revised .cabal from `package-description-override`)
  # is intentionally NOT a trigger for repacking: revisions are
  # carried by the slicing repo's `00-index.tar.gz` instead, leaving
  # the tarball bytes equal to upstream hackage so cabal computes
  # the same `pkgHashSourceHash` here as it would against real
  # hackage.  Patches and pre/postPatch hooks still go through the
  # repack path because they actually change source bytes.
  pkgTarball =
    if !package.isLocal && resolvedPatches == [] && prePatch == null && postPatch == null then src
    else runCommand "v2-tar-${pkgName}-${pkgVersion}"
           { preferLocalBuild = true; }
           # `cp -L` resolves symlinks against `${src}` at copy time.
           # Local source trees often have symlinks like
           # `lib/wallet/specifications -> ../../specifications`
           # (cardano-wallet) pointing outside the per-package subdir;
           # without `-L`, those symlinks survive into the tarball and
           # cabal-install 3.16+ rejects them at extract time as
           # [Cabal-7125] "Unsafe link target in tar archive".  Using
           # `-L` against the nix-store source resolves the targets
           # while we still have access to them, inlining the content.
           (if package.isLocal && pkgHasHook
            then ''
              workDir=$(mktemp -d)
              # Copy the full enclosing source (includes siblings) so
              # patches that reach outside the package dir still apply.
              srcParent=$(dirname ${src})
              pkgSubDir=$(basename ${src})
              cp -rL --no-preserve=mode $srcParent/. $workDir/
              chmod -R u+w $workDir
              target=$workDir/$pkgSubDir
              ${patchApplyScript}
              ${hpackScript}
              ${cabalFileOverride}
              # Rename the package subdir to match the tarball convention
              # so cabal sees `<name>-<version>/` when unpacking.
              mv $target $workDir/${pkgName}-${pkgVersion}
              tar --sort=name --mtime='@0' --owner=0 --group=0 --numeric-owner \
                  -czf $out -C $workDir ${pkgName}-${pkgVersion}
            ''
            else ''
              workDir=$(mktemp -d)
              target=$workDir/${pkgName}-${pkgVersion}
              mkdir -p $target
              ${if package.isLocal
                then "cp -rL ${src}/. $target/"
                else "tar -xzf ${src} --strip-components=1 -C $target"}
              ${patchApplyScript}
              ${hpackScript}
              ${cabalFileOverride}
              tar --sort=name --mtime='@0' --owner=0 --group=0 --numeric-owner \
                  -czf $out -C $workDir ${pkgName}-${pkgVersion}
            '');

  # Direct haskell library deps of this component.  Drop the
  # same-package self-reference that `lookupDependency` adds for
  # Custom build-type packages (a bare package record pointing at
  # ourselves), but keep sibling-component v2 slices that share
  # this package's name — those are sublibs (or sibling libraries)
  # that we genuinely depend on.  Slices are flagged via
  # `passthru.isSlice`; bare package records are not.
  directLibDeps = lib.filter (d:
    d ? identifier
    && ( (d.passthru.isSlice or false)        # sibling component, keep
         || d.identifier.name != pkgName       # external dep, keep
         # For a sublib, a same-pkg `hsPkgs.<pkg>` entry resolves to
         # the *main lib* (not this sublib), so it's a real upstream
         # dep we need composed in.  For the main lib slice itself
         # the same entry would be self-referential — drop it there.
         || isSublib
       )
  ) (component.depends or []);

  # Cabal's solver plans *every* library, sublib, and exe of every
  # package it sees — their build-depends all have to be solvable, not
  # just the ones we target.  Tests/benches are gated behind stanzas
  # and stay off by default, so their deps don't matter (unless we're
  # building a test/bench slice).  Gather deps from every sibling
  # component that the solver will plan, so the slicing repo has all
  # the tarballs cabal might need.
  #
  # Uses hsPkgs.<name>.components.<kind>.<n>.config.depends because
  # that's where plan.nix populates the resolved dep list — pkgSet
  # (the module config) doesn't carry it.
  # Cabal's solver plans every component of every package it sees,
  # so when the slice targets `:pkg:foo:lib:bar` the solver also has
  # to satisfy the deps of `foo`'s *other* components (library's
  # sublibs, exes, custom-setup's `setup-depends`, etc.).
  #
  # The current component's own deps come through
  # `component.depends` as pkg-derivation refs; sibling-component
  # deps come through `homeDependIds` as `{ name; version; }`
  # pairs (resolved from plan-json by hspkg-builder — kept as raw
  # strings so we don't have to force module options that could
  # reach back into our own construction).
  externalDeps = lib.filter
    (d: d ? identifier && d.identifier.name != pkgName)
    (component.depends or []);
  # Flattened `{ name; version; }` list of the solver's required
  # package-pins — current-component deps + sibling deps.  Each
  # entry has a top-level `.name`, so `uniqueWithName` partitions
  # by name and only falls back to a per-bucket scan for collisions.
  #
  # Sibling `exe-depends` (build-tools) are included on both native
  # and cross: cabal's solver requires the build-tool package (and
  # its lib closure) to be in the slicing repo's index just to
  # resolve `build-tool-depends: <pkg>:<exe>` as a plan goal —
  # `--with-PROG=PATH` only kicks in at build time, after the
  # solver has succeeded.  Leaving them out fails the solver with
  # `unknown package: <pkg>:<exe>:exe.<exe> (dependency of …)`.
  #
  # On NATIVE the plan-nix unit-ids match what the slice's cabal
  # computes, so cabal recognises the pre-installed tool in the
  # cabal-store and doesn't re-build.  On CROSS the unit-ids
  # diverge (cross GHC info ≠ build-platform GHC info), so cabal
  # would otherwise plan a from-source rebuild; `withProgFlags`
  # below adds `--with-<exe>=<bb-slice>/bin/<exe>` per transitive
  # build-tool so cabal short-circuits the build.  The post-install
  # "exactly one captured unit-id" check then verifies cabal really
  # did skip the tool's build.
  externalDepIds = haskellLib.uniqueWithName
    (map (d: { name = d.identifier.name; version = d.identifier.version; }) externalDeps
     ++ lib.filter (nv: nv.name != pkgName)
          (homeDependIds ++ homeBuildToolIds));

  # An entry in `component.depends` is either a dep package record
  # (has `.components`, e.g. `hsPkgs.provider`) or a v2 slice
  # itself (sublib components arrive this way under
  # `builderVersion = 2`, since `hsPkgs.<pkg>.components.sublibs.<n>`
  # is already a slice).  Handle both: prefer the package's
  # library when available, otherwise fall back to `d` itself if
  # it's already a slice.  This also avoids an `hsPkgs.<name>`
  # re-lookup, which can blow the eval stack on large projects
  # (ghc / hadrian).  The component IS the v2 slice when
  # `builderVersion = 2`.
  directDepSlices = haskellLib.uniqueWithName (map (d:
    let
      depLib = d.components.library or null;
      asSlice = if d.passthru.isSlice or false then d else null;
      slice = if depLib != null then depLib else asSlice;
    in if slice == null
        then throw "comp-v2-builder: ${pkgName}:${componentKindLabel}:${componentName} depends on ${d.identifier.name}, which has no v2 slice"
        else slice
  ) directLibDeps);

  # Packages with pre-2.0 Cabal-version share a single UnitId across
  # all their components (the pre-per-component install model), so
  # composing the library's v2 slice into the starting store for an
  # exe/test/bench would make cabal think the *exe* is also already
  # installed — it prints "Up to date" and skips the build.  For
  # those packages we fall back to building lib + exe together in
  # the same slice.
  #
  # `package.specVersion` is Cabal's parsed `cabal-version:` field.
  useSharedUnit =
    let v = package.specVersion or "0"; in
    builtins.compareVersions v "2.0" < 0;

  # For non-library components of modern Cabal-version packages,
  # compose the same-package library's v2 slice into the starting
  # store so cabal reuses it instead of rebuilding.  For shared-unit
  # (pre-2.0) packages we skip the pre-install (collides at UnitId)
  # but still want the library's transitive dep tarballs in the
  # slicing repo, so the solver can plan the library's `build-depends`
  # when cabal pulls it in via `extra-packages:`.
  ownLibPkg = hsPkgs.${pkgName} or null;
  ownLibV2 =
    if isLibrary || ownLibPkg == null
       || !(ownLibPkg ? components)
       || !(ownLibPkg.components ? library)
       || ownLibPkg.components.library == null
    then null
    else ownLibPkg.components.library;
  # Compose the package's own library slice into the starting store
  # for non-library components (exe / test / bench).  Sublibs pick
  # up the main lib through `directDepSlices` instead — only when
  # the sublib's `component.depends` actually references it (via
  # the `hsPkgs.<pkg>` package record).  Going through that path
  # avoids adding a sublib → main edge for sublibs that don't
  # depend on the main lib (e.g. `attoparsec:internal`); doing it
  # unconditionally would create a sublib ↔ main cycle whenever
  # the main lib re-exports the sublib.
  ownLibSlice =
    if isLibrary then null else ownLibV2;

  # Build-tool-depends: any of this component's `build-tools` that
  # resolved to a v2 exe slice (under `builderVersion = 2` this is
  # the common case for haskell-nix-provided tools like alex/happy).
  # Composing them into the starting store lets cabal's solver find
  # them as "already installed" — without that, the solver fails to
  # resolve `build-tool-depends: alex:alex` etc.
  buildToolSlices = lib.filter
    (t: t != null
        && t ? passthru
        && (t.passthru ? transitiveTarballs))
    extraNativeBuildInputs;

  # Transitive build-tool slices: every exe unit in this slice's
  # `allDepClosure` (which already follows `exe-depends`) whose slice
  # we can resolve via `hsPkgs.<pkg>.components.exes.<exe>`.  Without
  # composing these in, cabal's solver in the slice sees the exe in
  # `extra-packages:` (because it landed in `libConstraintPins` via
  # the same `allDepClosure` walk), can't find a matching unit-id in
  # the cabal-store, and rebuilds the whole package — wasted work
  # since our standalone slice already produced the prebuilt unit.
  # Concretely: typed-protocols transitively reaches `hsc2hs` via a
  # lib dep's `exe-depends`; before this, hsc2hs got rebuilt inside
  # every typed-protocols / consumer slice.
  # `{ name; pkgName; targetSlice; buildSlice }` records for every
  # transitive build-tool exe reached in `allDepClosure`.  Two slices
  # per tool serve different roles on cross:
  #
  #   * `targetSlice` — the v2 slice built FOR THIS PROJECT'S target
  #     (cross) hsPkgs.  Its plan-nix-computed unit-id matches what
  #     this slice's cabal v2-build will compute for the tool
  #     (same dummy-ghc / real-ghc parity verified by
  #     `tests.dummy-ghc-info`).  Composed into the starting
  #     cabal-store so cabal recognises the tool as already
  #     installed and skips re-building.  The slice's binary
  #     targets the cross platform — it isn't directly runnable on
  #     the build host — but cabal only needs the unit-id-keyed dir
  #     to be present in the store; actual invocation goes through
  #     `--with-PROG=PATH` below.
  #
  #   * `buildSlice` — the v2 slice built for the build-build
  #     platform (`hsPkgs.pkgsBuildBuild`).  Its binary is runnable
  #     on the build host.  Used for `--with-PROG=PATH` so cabal
  #     invokes a working binary when processing `.hsc` / `.x` /
  #     `.y` files in the slice.
  #
  # On NATIVE the two are the same slice (pkgsBuildBuild == hsPkgs).
  # The original concern about composing a cross-target exe was
  # that the build host's cabal can't EXECUTE it; with
  # `--with-PROG=<buildSlice>/bin/<exe>` cabal never tries to
  # execute the composed cross binary — it just notes its
  # existence at the expected unit-id path.
  transitiveBuildToolEntries =
    let raw = lib.concatMap (e:
      let p   = e.entry;
          pn  = p.pkg-name or "";
          cn  = p.component-name or "";
          cnPrefix = "exe:";
          isExe = lib.hasPrefix cnPrefix cn;
          exeName = builtins.substring (builtins.stringLength cnPrefix)
                      (builtins.stringLength cn - builtins.stringLength cnPrefix) cn;
          targetPkg = hsPkgs.${pn} or null;
          targetExes = if targetPkg != null && targetPkg ? components
                         && targetPkg.components ? exes
                       then targetPkg.components.exes else {};
          targetSlice = targetExes.${exeName} or null;
          buildPkg = hsPkgs.pkgsBuildBuild.${pn} or null;
          buildExes = if buildPkg != null && buildPkg ? components
                        && buildPkg.components ? exes
                      then buildPkg.components.exes else {};
          buildSlice = buildExes.${exeName} or null;
      in if pn != "" && pn != pkgName && isExe
            && targetSlice != null && buildSlice != null
         then [ { name = exeName; pkgName = pn; inherit targetSlice buildSlice; } ]
         else []
    ) allDepClosure;
    in haskellLib.uniqueWithName raw;

  # Slices composed into the starting cabal-store.  Cross-target
  # slices on cross (so cabal recognises the matching unit-id);
  # build-build slices on native (which is the same drv).
  transitiveBuildToolSlices = map (e: e.targetSlice) transitiveBuildToolEntries;

  # Cross-only: point cabal at the build-build binary for each
  # transitive build-tool — the composed `targetSlice` is the
  # cross-target binary which can't run on the build host.  Both
  # the slice's cabal v2-build and the v2 shell's cabal use these
  # flags so the runtime invocation path is consistent.
  withProgFlags =
    lib.optionalString isCross
      (lib.concatMapStrings
        (e: " --with-${e.name}=${e.buildSlice}/bin/${e.name}")
        transitiveBuildToolEntries);

  # Package names whose presence in the slice's plan / captured-unit
  # set is expected on cross.  Normally cabal recognises each
  # transitive build-tool's `targetSlice` (matching unit-id) and
  # neither plans nor builds it; this list is the safety net for
  # cases where a uid mismatch slips through (cabal would then
  # plan + build the tool, the unit would be captured here, and
  # we want to accept that rather than fail).  Empty on native.
  allowedBuildToolPackages =
    if isCross then lib.unique (map (e: e.pkgName) transitiveBuildToolEntries) else [];

  # Look up the v2 slice for a home-dep (sibling-component
  # gathered via plan.json).  Prefer the library, fall back to a
  # sublib, then to any exe — mirrors `depTransitiveTarballsOf`
  # below.  Sublib fallback covers packages whose `.cabal` only
  # defines named sublibraries (no main library) — e.g.
  # `cardano-wallet-ui` exports `cardano-wallet-ui:common` and
  # `cardano-wallet-ui:shelley` but has no main `library`.
  # Returns null when none is present (boot / pre-existing pkgs).
  homeDepSliceOf = nv:
    let dPkg = hsPkgs.${nv.name} or null;
        depLib = if dPkg != null
                    && (dPkg ? components)
                    && (dPkg.components ? library)
                    && (dPkg.components.library != null)
                 then dPkg.components.library else null;
        sublibsMap = if dPkg != null && (dPkg ? components) && (dPkg.components ? sublibs)
                     then dPkg.components.sublibs else {};
        anySublib = lib.findFirst (v: v != null) null
          (lib.attrValues sublibsMap);
        # Exe-only fallback (build-tool packages like hsc2hs / alex /
        # happy): look up the slice on the CROSS hsPkgs so the
        # composed unit-id matches what this slice's cabal computes
        # for the tool (cross dummy-ghc parity, see
        # `transitiveBuildToolEntries`).  The slice's cross-target
        # binary isn't runnable on the build host, but cabal only
        # needs the unit-id-keyed dir to be present in the
        # cabal-store; `withProgFlags` provides the build-build
        # binary cabal actually invokes.
        exesMap = if dPkg != null && (dPkg ? components) && (dPkg.components ? exes)
                  then dPkg.components.exes else {};
        anyExe = lib.findFirst (v: v != null) null
          (lib.attrValues exesMap);
    in if depLib != null then depLib
       else if anySublib != null then anySublib
       else anyExe;
  homeDepSlices = lib.filter (s: s != null)
    (map homeDepSliceOf externalDepIds);

  # The slice's *direct* dep slices.  Transitive closure is
  # reconstructed at build time from each slice's
  # `$out/nix-support/transitive-deps` file (see
  # `build-cabal-slice.nix`), so we deliberately don't expand it
  # here — keeping the nix-side eval cheap and the buildInputs list
  # short.
  depSlices = haskellLib.uniqueWithName
    (directDepSlices
     ++ lib.optional (ownLibSlice != null) ownLibSlice
     ++ buildToolSlices
     ++ transitiveBuildToolSlices
     ++ homeDepSlices);

  # `constraints:` block in cabal.project pinning every transitive
  # dep — both the target package's lib closure and the lib closures
  # of build tools (alex / happy / hsc2hs / …).  Without this,
  # cabal's solver in this slice freely picks GHC-bundled `-inplace`
  # versions when the version range allows, but the build-tool
  # slices were originally built against the *reinstalled* versions
  # (e.g. hsc2hs's slice built against `process-1.6.28.0`, not
  # GHC-bundled `process-1.6.26.1-inplace`).  The dep unit-ids
  # diverge, so hsc2hs's computed unit-id in this slice doesn't
  # match the one composed via lndir from the hsc2hs slice's $out,
  # and cabal plans a from-source rebuild instead of reusing the
  # binary already on PATH.
  #
  # Scoping to `depTarballsDeduped` (per-slice) bounds the drv-hash
  # blast radius: only slices whose closure includes a given
  # package are affected by that package's version change.
  #
  # Emits per-pkg constraint lines:
  #   * `==<ver>`     — pin the version
  #   * `source`      — force pickup from the slicing repo, not the
  #                     inplace (GHC-bundled) copy.  Without `source`,
  #                     cabal happily uses `time-1.15-inplace` when
  #                     plan-nix's outer solve had `time-1.15-9fa08573`
  #                     (configured/reinstalled at the same version),
  #                     because `==1.15` alone doesn't disambiguate
  #                     inplace from source.
  # `source` is only emitted for non-`pre-existing` packages: GHC's
  # boot pkgs (base, ghc-prim, template-haskell, ...) have no source
  # in the slicing repo and the inplace copy is the only candidate.
  # Pre-existing entries are skipped entirely (no version pin either)
  # — GHC's bundled package db already fixes them, and pinning them
  # tends to push cabal off legacy-tool PATH fallbacks for hsc2hs /
  # alex / happy / etc.  Sourced from `libDepClosure` (lib deps
  # only); exe-deps are left unconstrained so build-tools can resolve
  # against their own slice's solve.
  libConstraintPins =
    let
      grouped = lib.foldl' (acc: e:
        let p = e.entry;
            n = p.pkg-name or "";
            v = p.pkg-version or "";
        in if n == "" || n == pkgName
              || acc ? ${n}
              || (p.type or null) == "pre-existing"
           then acc
           else acc // { ${n} = v; }
      ) {} libDepClosure;
    in lib.sort (a: b: a.name < b.name)
        (lib.mapAttrsToList (n: v: { name = n; version = v; }) grouped);

  depConstraints =
    "constraints: any.${pkgName} source\n"
    + lib.concatMapStrings
        (e: "constraints: ${e.name} ==${e.version}, ${e.name} source\n")
        libConstraintPins;

  # Transitive tarballs of a dep pkg — prefer the library's v2 slice
  # (covers the normal case), fall back to any sublib's v2 slice
  # (for packages that only define `library <name>` stanzas, e.g.
  # `cardano-wallet-ui:{common,shelley}`), then to any exe's v2
  # slice (for exe-only deps like hsc2hs / alex / happy).  Every
  # v2 slice carries a `transitiveTarballs` passthru that expands
  # to all source tarballs the slice's cabal invocation needed —
  # including the pkg's own source — so any of these routes
  # gives us a superset that satisfies our current slice's solver.
  depTransitiveTarballsOf = nv:
    let
      dPkg = hsPkgs.${nv.name} or null;
      depLib = if dPkg != null
                  && (dPkg ? components)
                  && (dPkg.components ? library)
                  && (dPkg.components.library != null)
               then dPkg.components.library else null;
      sublibsMap = if dPkg != null && (dPkg ? components) && (dPkg.components ? sublibs)
                   then dPkg.components.sublibs else {};
      anySublib = lib.findFirst (v: v != null) null
        (lib.attrValues sublibsMap);
      exesMap = if dPkg != null && (dPkg ? components) && (dPkg.components ? exes)
                then dPkg.components.exes else {};
      anyExe = lib.findFirst (v: v != null) null
        (lib.attrValues exesMap);
    in if depLib != null
       then depLib.transitiveTarballs
       else if anySublib != null
       then anySublib.transitiveTarballs
       else if anyExe != null
       then anyExe.transitiveTarballs
       else throw "comp-v2-builder: ${pkgName}:${componentKindLabel}:${componentName} (sibling-dep) references ${nv.name}, which has no v2 slice";

  depTransitives =
    lib.concatMap depTransitiveTarballsOf externalDepIds
    # Always include the same-package library's transitive tarballs
    # when we're building a non-library component — cabal's solve
    # for `:pkg:foo:exe:...` still plans the library, and its
    # `build-depends` need to be satisfiable from the repo.  (Even
    # for useSharedUnit where we don't pre-install the lib.)
    ++ lib.optionals (ownLibV2 != null) ownLibV2.transitiveTarballs
    # The real package's tarball is always added to the slicing
    # repo: cabal pulls it in via `extra-packages:` and the slice's
    # `:pkg:foo:lib:bar`-style cabal target, so the package has to
    # be reachable through the file:// repo's `00-index.tar.gz`.
    ++ [ { name = pkgName; version = pkgVersion; tarball = pkgTarball; cabalFile = thisCabalFile; } ]
    # Include each direct build-tool's source tarball (plus its
    # lib deps) too — cabal's solver in the slice always needs the
    # build-tool package (and its full lib closure) in the index to
    # resolve `build-tool-depends:` goals, even when the unit is
    # already in the starting cabal-store or pinned via
    # `--with-PROG=PATH`.  On native cabal then plans + reuses the
    # pre-installed slice; on cross `withProgFlags` short-circuits
    # the build.
    ++ lib.concatMap (s: s.passthru.transitiveTarballs or []) buildToolSlices;

  # native-build inputs from the target component's config
  libs       = lib.flatten (component.libs or []);
  frameworks = component.frameworks or [];
  pkgconfig  = map pkgs.lib.getDev (builtins.concatLists (component.pkgconfig or []));

  # Paths post-injected into this lib slice's registered `.conf` so
  # GHC's runtime linker can dlopen each `extra-libraries:` entry
  # under TH eval / `-fexternal-interpreter`.  Mirrors v1's
  # `make-config-files.nix:flagsAndConfig "extra-lib-dirs"` — same
  # set (component.libs' `lib/` outputs), but injected post-register
  # instead of pre-configure to keep the slice's unit-id equal to
  # plan-nix's (configure-time `--extra-lib-dirs` lands in
  # `pkgHashExtraLibDirs` and would diverge).  See
  # `build-cabal-slice.nix:confLibraryDirs` for the rewrite.
  confLibraryDirs =
    map (p: "${lib.getLib p}/lib") libs
    ++ lib.optionals stdenv.hostPlatform.isWindows
         # Windows ships DLLs / import libs under `bin/` rather
         # than `lib/`.  Mirrors v1's runtime `[ -d $bin ]` check
         # in `make-config-files.nix:81-90`.
         (map (p: "${lib.getBin p}/bin") libs);
  buildTools = component.build-tools or [];
  resolvedBuildTools = lib.concatMap (p:
    let resolved = if builtins.isFunction p
          then p { inherit (package.identifier) version; }
          else p;
    in if resolved == null then []
       else if resolved.isHaskell or false
         then builtins.attrValues (resolved.components.exes or {})
       else [ resolved ]
  ) (lib.filter (p: p != null) buildTools);

  # Resolve a home-dep {name;version} (sibling-component from
  # plan-json, includes setup-depends) to the same "pkg record"
  # shape that component.depends gives us.
  homeDepPkgOf = nv: hsPkgs.${nv.name} or null;
  homeDepPkgs = lib.filter (p: p != null) (map homeDepPkgOf homeDependIds);

  # SysLibs of a component — the same shape we feed to
  # `extraBuildInputs` and `cabal.project`'s `extra-include-dirs:`.
  sysLibsOf = c:
    (lib.flatten (c.libs or []))
    ++ (c.frameworks or [])
    ++ (map pkgs.lib.getDev (builtins.concatLists (c.pkgconfig or [])));

  # Walk the lib-dep tree to collect every transitive `libs` /
  # `frameworks` / `pkgconfig` entry (deduped by canonical pkg
  # name).  Mostly redundant now that `depSlices` are in
  # `propagatedBuildInputs` (see `build-cabal-slice.nix`) — stdenv's
  # chain already carries each slice's own `propagated` sysLibs to
  # consumers, and normal nixpkgs deps like `lmdb` / `liburing` /
  # `gmp` carry `__spliced` so they auto-swap to the consumer's
  # pkg-set under cross-compilation.  We keep the manual walk as a
  # belt-and-braces backup that covers cases the chain misses
  # (e.g. a sublib's pkgconfig dep that the main lib's slice
  # doesn't propagate).
  transitiveDepLibs =
    let
      go = acc: deps:
        lib.foldl' (acc: d:
          let
            name = d.identifier.name or "";
            libComp = d.components.library.config or null;
            childSysLibs = if libComp == null then [] else sysLibsOf libComp;
            childDeps    = if libComp == null then [] else (libComp.depends or []);
          in if name == "" || lib.elem name acc.seen
             then acc
             else let acc' = {
                        seen = acc.seen ++ [ name ];
                        libs = acc.libs ++ childSysLibs;
                      };
                  in go acc' childDeps
        ) acc deps;
    in (go { seen = [ pkgName ]; libs = []; }
         (directLibDeps ++ homeDepPkgs)).libs;

  # When the host needs a wrapped ghc for cross-TH (windows), the
  # wineIservWrapper symlinks `*.dll` files out of every path in
  # `pkgsHostTarget` into REMOTE_ISERV before launching wine.  For
  # `iserv-proxy-interpreter.exe`'s own runtime DLLs to be visible
  # there, the libs that linked it (mingw_w64_pthreads, gmp, ...)
  # must appear in our slice's buildInputs.  `templateHaskell.iservRuntimeLibs`
  # carries that list.
  iservRuntimeLibs =
    if templateHaskell != null && templateHaskell.iservRuntimeLibs or null != null
    then templateHaskell.iservRuntimeLibs
    else [];
  # Direct sysLibs plus the transitive closure walked above.  The
  # transitive set is mostly redundant given the `propagatedBuildInputs`
  # chain on `depSlices`, but it's cheap to keep as a defence against
  # gaps in the chain (e.g. when a slice's main lib doesn't carry the
  # pkgconfig deps of one of its sublibs forward).
  extraBuildInputs = haskellLib.uniqueWithName
    (lib.filter (x: x != null)
      (libs ++ frameworks ++ pkgconfig ++ transitiveDepLibs ++ iservRuntimeLibs));

  # Mirror v1's `make-config-files.nix:65-85`: emit cabal.project
  # `extra-include-dirs:` and `extra-lib-dirs:` for every C-lib /
  # framework / pkgconfig dep so cabal's foreign-library check at
  # configure time can find headers and `.a`/`.so`/`.dll.a` files.
  # cabal/v2-build doesn't auto-derive these from `buildInputs`
  # (unlike `setup configure` which gets them via NIX_LDFLAGS), so
  # we have to spell them out.
  #
  # Emit per-`package <name>` blocks rather than project-level —
  # cabal's `Distribution.Simple.Configure.checkForeignDeps` probe
  # only picks up the per-package values; project-level
  # `extra-lib-dirs:` is ignored by that probe.
  #
  # Critically, each dep's block must list *that dep's* foreign
  # libs — not the union — because each entry enters
  # `pkgHashExtraIncludeDirs` / `pkgHashExtraLibDirs` of the
  # named package.  Mirroring what each dep emitted in *its own*
  # slice keeps the dep's UnitId stable across slices.  Otherwise
  # cabal sees a hash mismatch and rebuilds the dep from source.
  dirsLinesOf = pkgs:
    let nonEmpty = haskellLib.uniqueWithName (lib.filter (x: x != null) pkgs); in
    if nonEmpty == [] then ""
    else
      lib.concatMapStrings (p: "    extra-include-dirs: ${lib.getDev p}/include\n") nonEmpty
      + lib.concatMapStrings (p: "    extra-lib-dirs: ${lib.getLib p}/lib\n") nonEmpty
      # On windows host, libraries often ship the `.dll` and import
      # lib (`.dll.a`) under `bin/` rather than `lib/` (e.g. our
      # test-clib).  Mirror v1's runtime `[ -d $bin ]` check by
      # always emitting the `bin/` entry under windows host —
      # cabal tolerates non-existent search dirs.
      + lib.optionalString stdenv.hostPlatform.isWindows
          (lib.concatMapStrings (p: "    extra-lib-dirs: ${lib.getBin p}/bin\n") nonEmpty);

  # Map of canonical-pkg-name → that pkg's own foreign libs.  Built
  # by walking the dep tree (`directLibDeps ++ homeDepPkgs`) keyed
  # by package name.  The target itself is keyed on `pkgName`.
  #
  # `pkgconfig` deps and plain `c.libs` are deliberately excluded
  # here — both go into `extraBuildInputs` so the slice's wrapped
  # cc / pkg-config can find them at build time, but neither must
  # appear in cabal.project's `extra-include-dirs:` /
  # `extra-lib-dirs:` lines: those land in
  # `pkgHashExtraIncludeDirs` / `pkgHashExtraLibDirs` and would put
  # `/nix/store/<hash>-<lib>-dev/include` paths into the unit-id
  # hash.  plan-to-nix's compute never sees those paths (its
  # cabal.project is just the user's; haskell.nix's `c.libs`
  # overrides come from modules evaluated *after* plan-nix), so
  # any line here forks the unit-id from plan-nix and breaks the
  # checkAgainstPlan check (e.g. `unix-time` on windows cross,
  # which gets `c.libs = [mingw_w64_pthreads]` from
  # `overlays/windows.nix`).  cabal still finds these libs at
  # configure / build time: `pkgconfig` via the wrapped pkg-config,
  # plain `c.libs` via the wrapped cc's NIX_CFLAGS_COMPILE_FOR_TARGET
  # / NIX_LDFLAGS_FOR_TARGET (every entry here is also in
  # `extraBuildInputs`).
  #
  # `c.frameworks` is kept because Darwin frameworks live under
  # `Library/Frameworks/`, not the standard `include/`/`lib/`
  # layout the cc wrapper expects, so cabal really does need
  # `extra-framework-dirs:` to find them.  The unit-id divergence
  # only affects Darwin host builds.
  pkgcfglessSysLibs = c:
    (c.frameworks or []);
  perPkgForeignLibs =
    let
      go = acc: deps:
        lib.foldl' (acc: d:
          let
            name = d.identifier.name or "";
            libComp = d.components.library.config or null;
            childLibs = if libComp == null then [] else pkgcfglessSysLibs libComp;
            childDeps = if libComp == null then [] else (libComp.depends or []);
          in if name == "" || acc ? ${name}
             then acc
             else go (acc // { ${name} = childLibs; }) childDeps
        ) acc deps;
      # Same exclusions as `pkgcfglessSysLibs` above: only frameworks
      # need cabal-side `extra-framework-dirs:` entries; plain `libs`
      # are picked up by the wrapped cc and would fork unit-ids if
      # listed here.
      selfLibs = haskellLib.uniqueWithName frameworks;
      initial  = { ${pkgName} = selfLibs; };
    in go initial (directLibDeps ++ homeDepPkgs);

  extraIncludeAndLibDirs = lib.concatStrings
    (lib.mapAttrsToList (n: ds:
      let body = dirsLinesOf ds;
      in if body == "" then "" else "package ${n}\n${body}"
    ) perPkgForeignLibs);
  # When we don't pre-install the same-package library into the
  # exe slice's starting store (see ownLibSlice), cabal's plan will
  # re-build the library alongside the exe.  The library may have
  # its own `build-tools` (e.g. alex / happy) that need to be on
  # PATH during its configure step.  Add every home-dep that
  # resolves to an exe slice to `extraNativeBuildInputs` so these
  # tools are available regardless of which pkg components cabal
  # ends up planning.
  #
  # Look the exe up via `hsPkgs.pkgsBuildBuild` rather than the
  # host `hsPkgs` — `homeBuildToolIds` is the sibling-component
  # `exe-depends` set from plan-json (the build-tool-depends
  # entries).  Without this we'd pull in the cross-compiled exe
  # (e.g. `alex.exe`) which can't run on the build platform; v1's
  # `lookupExeDependency` already uses `pkgsBuildBuild` for the
  # same reason.
  homeDepExeSlices =
    let
      exesOf = nv:
        let p = hsPkgs.pkgsBuildBuild.${nv.name} or null;
        in if p != null && (p ? components) && (p.components ? exes)
           then lib.filter (v: v != null) (lib.attrValues p.components.exes)
           else [];
    in lib.concatMap exesOf homeBuildToolIds;

  extraNativeBuildInputs = haskellLib.uniqueWithName (lib.filter
    (t: t != null)
    (resolvedBuildTools ++ homeDepExeSlices));

  # ---- componentKindLabel / componentName -----------------------
  # `componentId` is a set with `ctype` (e.g. "lib", "exe", "test",
  # "bench", "sublib") and `cname`.
  ctype = componentId.ctype;
  cname = componentId.cname;
  componentKindLabel = ctype;
  componentName = cname;

  # Platform-specific executable extension.  Wasm produces
  # `.wasm`; native Windows produces `.exe`; everything else
  # (including ghcjs under v2) has no extension.
  #
  # v1's `comp-builder.nix:450` appends `.jsexe/all.js` for
  # `isGhcjs && ghc < 9.8`, matching the directory layout that
  # `Setup.hs install` preserves.  v2 builds via `cabal v2-build`,
  # whose install step *bundles* the `.jsexe/` contents into a
  # single self-contained `#!/usr/bin/env node` script at
  # `bin/<cname>` — there is no `.jsexe/` directory in the slice's
  # output.  Use the bundled-file path here so v2's exePath /
  # `find` lookups land on the file cabal actually produced.
  exeExt =
    if stdenv.hostPlatform.isWasm
      then ".wasm"
    else stdenv.hostPlatform.extensions.executable;
  exeName = cname + exeExt;

  # ---- per-kind shape ------------------------------------------
  # `componentPrefix.sublibs == "lib"` (haskellLib in lib/default.nix),
  # so a sublib component arrives here with `ctype == "lib"` too.
  # Distinguish by component name: the main library has `cname ==
  # package-name`, sublibs have any other name.
  isMainLib  = ctype == "lib" && cname == pkgName;
  isSublib   = ctype == "lib" && cname != pkgName;
  isLibrary   = isMainLib || isSublib;
  # Native exe slices ship the binary inside the cabal-store unit
  # dir under `$out/store/.../<unit>/bin/`.  Cross exe slices target
  # `<exe>.exe` (or platform equivalent) which can't run on the
  # build platform anyway, so we capture from `dist-newstyle/`
  # instead.  Same shape as test/bench slices.
  useTarball  = isLibrary || ctype == "exe";
  targetPrefix =
    if ctype == "lib"   then "lib:"  # main lib + sublib both arrive as ctype="lib"
    else if ctype == "exe"   then "exe:"
    else if ctype == "test"  then "test:"
    else "bench:";
  extraProject =
    if ctype == "test"  then "tests: True\nbenchmarks: False\n"
    else if ctype == "bench" then "tests: False\nbenchmarks: True\n"
    else "tests: False\nbenchmarks: False\n";

  # Test / benchmark slices need the target package treated as
  # local: cabal refuses with `Cabal-7127` to build test or
  # benchmark components on a non-local package, so the
  # `extra-packages:` shape that lib / exe slices use isn't
  # available.  Local-style unit-ids fork from
  # `extra-packages:`-style ones, but plan-nix records local
  # packages with `style: "local"` too — so `expectedUnitId`
  # below already skips the unit-id check for them, and
  # `checkAgainstPlan` (the plan-entry diff diagnostic) is the
  # right tool for catching real divergences.
  #
  # Limitation: a non-local package's test or bench (e.g. a
  # hackage dep that ships a test-suite) can't be sliced this
  # way — cabal-7127 would still trigger.  In practice we only
  # build tests / benches for the project's own packages.
  isLocalTestOrBench = (package.isLocal or false) && (ctype == "test" || ctype == "bench");

  # The slice targets the package's component directly via cabal's
  # `:pkg:` qualifier (`:pkg:foo:lib:bar`).  cabal-install bug #8684
  # makes the bare form fail with "unambiguous but matches the
  # following targets" when the package isn't in `packages:`, so the
  # qualifier is always required.  `extra-packages:` adds the
  # package to the solver as a remote candidate without treating it
  # as a local package (which would force `inplace` unit-ids that
  # don't match plan-nix).  No local "shim" package or `packages:`
  # entry is needed — the slicing repo + `extra-packages:` is
  # everything cabal v2-build's solver needs.
  #
  # `planJsonByPlanId` arrives pre-built from the project-level
  # `config.plan-json-by-id` (see hspkg-builder).  Used to be
  # rebuilt per slice, which on big projects exploded the
  # evaluator's heap to multiple GiB.

  # Plan units the dep closure should be seeded from.  For lib slices
  # we narrow to *just this slice's own unit* (when `package.identifier.unit-id`
  # is known and present in the plan) — otherwise an unrelated sublib's
  # deps (e.g. `lib:testlib` pulling in `nothunks` / `QuickCheck`) leak
  # into `libConstraintPins` for an unrelated sublib slice (e.g.
  # `lib:si-timers`), and cabal's plan validator rejects the resulting
  # config with "the package configuration specifies <pkg> but (with
  # the given flag assignment) the package does not actually depend
  # on any version of that package".  Local test / bench slices and
  # the rare case of a missing unit-id keep the old "every unit of
  # this package" seed (still correct for those shapes).
  ourPlanUnits =
    let allUnits = lib.filter
          (p: (p.pkg-name or null) == package.identifier.name
              && (p.pkg-version or null) == package.identifier.version)
          planJson;
        uid = package.identifier.unit-id or null;
        myUnit = if uid == null then null else planJsonByPlanId.${uid} or null;
    in if isLibrary && myUnit != null then [ myUnit ] else allUnits;

  # `depends` only — for the cabal.project `constraints:` block
  # below, where lib-dep closure entries get version-pinned to the
  # exact unit plan-nix recorded.  exe-depends are deliberately
  # skipped: exes (alex / happy / hsc2hs / ...) routinely pick
  # different versions of their lib deps than the libs in the
  # slice, and pinning them in this slice's constraints would
  # wrongly cap the exe's own solve.
  libDepsOf = p:
    (p.depends or [])
    ++ lib.concatMap (c: c.depends or [])
         (lib.attrValues (p.components or {}));

  # `depends` + `exe-depends` — for the slicing repo's tarball set
  # and the slice's dep-slice composition.  cabal needs every
  # build-tool package (and the tool's lib closure) reachable in
  # the index AND composed into the starting store.
  allDepsOf = p:
    (p.depends or [])
    ++ (p.exe-depends or [])
    ++ lib.concatMap (c: (c.depends or []) ++ (c.exe-depends or []))
         (lib.attrValues (p.components or {}));

  mkClosureFrom = seedUnits: depFn: builtins.genericClosure {
    startSet = lib.concatMap (p:
      lib.concatMap (id:
        let q = planJsonByPlanId.${id} or null;
        in if q == null then [] else [ { key = id; entry = q; } ]
      ) (depFn p)
    ) seedUnits;
    operator = e:
      lib.concatMap (id:
        let q = planJsonByPlanId.${id} or null;
        in if q == null then [] else [ { key = id; entry = q; } ]
      ) (depFn e.entry);
  };
  mkClosure = mkClosureFrom ourPlanUnits;
  libDepClosure = mkClosure libDepsOf;
  allDepClosure = mkClosure allDepsOf;
  # Seeded from *every* unit of this package (main lib + every
  # sublib + exes/tests/benches), not just this slice's unit.  Used
  # for `extraSublibSeeds`: cabal-install's `prune-unreachable-sublibs`
  # patch validates the *whole* package's flagged_deps against the
  # solver index, so even when this slice targets `:pkg:foo:lib:control`,
  # cabal still verifies that `foo:lib:core`'s deps (e.g.
  # `io-classes:strict-mvar`) resolve.  Narrowing this to
  # `ourPlanUnits` would silently drop sublib seeds the validator
  # still needs.
  pkgPlanUnits = lib.filter
    (p: (p.pkg-name or null) == package.identifier.name
        && (p.pkg-version or null) == package.identifier.version)
    planJson;
  pkgLibDepClosure = mkClosureFrom pkgPlanUnits libDepsOf;

  # `:pkg:` qualifier disambiguates remote-package targets — cabal
  # bug #8684 makes the bare form fail with "unambiguous but
  # matches the following targets" when the package isn't in
  # `packages:`.
  targetSelector = ":pkg:${pkgName}:${targetPrefix}${cname}";

  # Sublibs (of THIS package or of any dep package) we need to
  # tell our patched cabal-install to keep as reachability seeds.
  # Without this, the `prune-unreachable-sublibs` patch drops every
  # sublib that the package's main lib doesn't directly reference,
  # and downstream slices fail to build (cabal-7127 for direct
  # targets, "Dependency on unbuildable library" for transitives).
  # Each entry is `{ pkg; sublib; }`.
  extraSublibSeeds =
    # The slice's own target sublib (when this slice targets one).
    lib.optional isSublib { pkg = pkgName; sublib = cname; }
    # Every sublib unit referenced by *any* plan unit of *any*
    # package in this slice's slicing repo.  Why this scope:
    # cabal-install's `prune-unreachable-sublibs` patch validates
    # every package whose cabal file lives in the index — not just
    # the install plan — so even a pkg like `strict-checked-vars`
    # that's only a transitive `transitiveTarballs` dep (no plan
    # unit in *this* slice's plan) still gets its sublib refs
    # checked.  We seed every sublib referenced by any unit of any
    # repo pkg so the patch keeps them all reachable.  Scoping to
    # `depTarballsDeduped` (per-slice content) bounds drv-hash
    # sensitivity: a new sublib only invalidates slices whose
    # slicing repo actually includes that package.
    ++ (let
          repoPkgNames = lib.unique
            (map (e: e.name) (lib.attrValues depTarballsDeduped));
          repoPkgNameSet = lib.genAttrs repoPkgNames (_: true);
          repoPlanUnits = lib.filter
            (p: repoPkgNameSet ? ${p.pkg-name or ""})
            planJson;
          allDepIds = lib.concatMap (p: p.depends or []) repoPlanUnits;
        in lib.concatMap (id:
             let q  = planJsonByPlanId.${id} or null;
                 pn = if q == null then "" else q.pkg-name or "";
                 cn = if q == null then "" else q.component-name or "";
                 prefix = "lib:";
                 prefixLen = builtins.stringLength prefix;
                 isSublibCN = lib.hasPrefix prefix cn;
                 sub = builtins.substring prefixLen
                         (builtins.stringLength cn - prefixLen) cn;
             in if pn != "" && isSublibCN
                then [{ pkg = pn; sublib = sub; }]
                else []
           ) allDepIds);

  # ---- Slicing repo (deps only — target served via packages:) ---
  depTarballsDeduped = lib.foldl' (acc: e:
    let key = "${e.name}-${e.version}";
    in if acc ? ${key} then acc else acc // { ${key} = e; }
  ) {} depTransitives;

  # Per-slice hackage-style repository:
  #
  #   $out/00-index.tar.gz                       cabal's package index
  #   $out/package/<pkg>-<ver>.tar.gz            source tarballs
  #
  # Why per-slice and not project-wide: a project-wide index is
  # touched by every cabal-file revision in the plan, so any
  # revision change would mass-invalidate every slice.  Per-slice
  # the index covers only this slice's dep set, so a revision change
  # only affects slices whose closure includes that package.
  #
  # Why hackage-style and not `file+noindex://`:
  # `file+noindex://` makes cabal hash the tarball *bytes* into
  # `pkgHashSourceHash`.  Any tarball repacking (mtime, sort order,
  # gzip metadata) forks the unit-id even when the unpacked source
  # is byte-identical.  Real hackage hashes the tarball as fetched
  # (no repack) and tracks revisions via the index — which is what
  # we want here so that `cabal v2-build <foo>` inside the dev shell
  # produces the same UnitId as the slice that pre-built `<foo>`.
  #
  # The .cabal staged into the index for each entry comes from:
  #   * `e.cabalFile`  — store path of the X-revised cabal-file when
  #                      `package-description-override` was set, OR
  #   * extracted from `e.tarball` at build time when there is no
  #                      override (revision-0 case).
  # In the latter case the index .cabal is byte-identical to the
  # tarball's, so cabal sees revision-0 and `pkgHashPkgDescriptionHash`
  # stays Nothing — matching what real hackage produces.
  # Symlink the tarballs (rather than copying) so the repo drv stays
  # tiny — each `package/<pkg>-<ver>.tar.gz` is a `/nix/store` link
  # into the original tarball derivation.  cabal reads the tarball
  # through the symlink transparently.
  repoCopyCmds = lib.concatStrings (lib.mapAttrsToList (_: e:
    "ln -s ${e.tarball} $out/package/${e.name}-${e.version}.tar.gz\n"
  ) depTarballsDeduped);

  indexEntryCmds = lib.concatStrings (lib.mapAttrsToList (_: e:
    let dir = "$workDir/${e.name}/${e.version}"; in
    if (e.cabalFile or null) != null then ''
      mkdir -p ${dir}
      cp ${e.cabalFile} ${dir}/${e.name}.cabal
    '' else ''
      mkdir -p ${dir}
      tar -xOzf ${e.tarball} \
        ${e.name}-${e.version}/${e.name}.cabal \
        > ${dir}/${e.name}.cabal
    ''
  ) depTarballsDeduped);

  slicingRepo = runCommand "v2-${componentKindLabel}-repo-${pkgName}-${componentName}"
    { preferLocalBuild = true; }
    ''
      mkdir -p $out/package
      ${repoCopyCmds}
      workDir=$(mktemp -d)
      ${indexEntryCmds}
      tar --sort=name --mtime='@0' --owner=0 --group=0 --numeric-owner \
          -czf $out/00-index.tar.gz -C $workDir .
    '';

  # Our slicing repo is already pinned to the exact versions plan.nix
  # chose (via per-package `constraints:` lines below) and uses
  # `active-repositories: hackage.haskell-nix, :none` so the solver
  # can't reach the user's real hackage.  Each package therefore has
  # exactly one candidate version — so `*:*` allow-newer can't
  # trigger a "wrong" choice; it just stops cabal from rejecting that
  # one candidate over stale upper bounds in the stock .cabal files
  # (which plan.nix itself worked around via head.hackage revisions,
  # allow-newer in the project, etc.).
  allowNewerBlock = "allow-newer: *:*\n";

  # `allow-boot-library-installs: True` overrides cabal-install's
  # hard-coded non-reinstallable list (`ghc`, `template-haskell`,
  # `Cabal`, `base`, …).  Needed when this slice's plan reinstalls
  # any of those packages — typically reached via
  # `useLocalGhcLib = true`, which delivers `lib:ghc` as a
  # source-repository-package.  Without the flag, cabal rejects the
  # source instance with "constraint from non-reinstallable package
  # requires installed instance".
  #
  # Gate on whether this slice's target or any of its non-pre-existing
  # lib deps is on the boot-lib list.  `libConstraintPins` already
  # filters out pre-existing pkgs (boot pkgs of an unmodified GHC
  # never appear here).
  bootLibPkgNames = [
    "ghc" "template-haskell" "Cabal" "Cabal-syntax"
    "ghc-prim" "ghc-bignum" "ghc-boot" "ghc-boot-th" "ghc-heap"
    "base" "ghci" "ghc-internal" "rts"
  ];
  closureHasReinstalledBootLib =
    lib.elem pkgName bootLibPkgNames
    || lib.any (e: lib.elem e.name bootLibPkgNames) libConstraintPins;
  allowBootLibBlock =
    lib.optionalString closureHasReinstalledBootLib
      "allow-boot-library-installs: True\n";

  # Project-level and per-package cabal.project pragmas extracted
  # from plan.json's `configure-args`.  Plan-to-nix runs
  # cabal-install with a specific set of `--enable-X`/`--disable-X`
  # toggles (--disable-shared, --disable-static,
  # --disable-library-profiling, --enable-optimization, ...) —
  # these enter cabal's `pkgHashConfigInputs` and so the unit-id
  # hash of every package the project plan recorded.  If the slice's
  # `cabal v2-build` uses different defaults (e.g. cabal's default
  # `--enable-shared` on darwin), the slice produces a different
  # unit-id even when name + version + deps are identical.
  #
  # Mirroring the same toggles in the slice's cabal.project keeps
  # `pkgHashConfigInputs` aligned with plan-nix.  Each plan entry's
  # `configure-args` is per-unit; we group by pkg-name (taking the
  # union of pragmas across units of the same package, since
  # cabal.project only supports per-package granularity) and:
  #
  #   * Emit a `package *` block with the *baseline* — pragmas
  #     present for every pkg-name.  Applies to transitive hackage
  #     deps the slice's `cabal v2-build` resolves fresh, so their
  #     UnitIds line up with plan-nix.
  #
  #   * Emit a `package <name>` block for every pkg-name whose
  #     pragmas extend the baseline (e.g. `package cabal-simple`
  #     getting `profiling: True` from
  #     `packages.cabal-simple.enableProfiling = true`).
  projectConfigPragmas =
    let
      configuredEntries = lib.filter (p: (p.type or "") == "configured") planJson;

      # Whitelist of cabal.project field names that map 1:1 to
      # `--enable-X` / `--disable-X` / `--X=value` Setup configure
      # args.  Keeping this explicit means we don't silently round-trip
      # something cabal.project rejects (e.g. `--cid=`, `--dependency=`,
      # `--with-ghc=` are NOT in this set).
      isProjectField = field: lib.elem field [
        "shared" "static"
        "library-vanilla" "library-profiling"
        "executable-dynamic" "executable-static"
        "profiling" "profiling-shared"
        "optimization" "debug-info" "build-info"
        "library-for-ghci"
        "split-sections" "split-objs"
        "executable-stripping" "library-stripping"
        "coverage" "relocatable"
        "profiling-detail" "library-profiling-detail"
      ];
      pragmaOf = arg:
        let
          en = builtins.match "--enable-([a-z0-9-]+)" arg;
          di = builtins.match "--disable-([a-z0-9-]+)" arg;
          kv = builtins.match "--([a-z0-9-]+)=(.+)" arg;
        in
          if      en != null && isProjectField (builtins.head en) then "${builtins.head en}: True"
          else if di != null && isProjectField (builtins.head di) then "${builtins.head di}: False"
          else if kv != null && isProjectField (builtins.head kv) then "${builtins.head kv}: ${builtins.elemAt kv 1}"
          else null;
      pragmasOf = args: lib.unique (lib.filter (s: s != null) (map pragmaOf args));

      # pkg-name → union of pragmas across every unit of that package.
      pragmasByName = lib.foldl' (acc: e:
        let
          name = e.pkg-name or "";
          ps = pragmasOf (e.configure-args or []);
        in if name == "" then acc
           else acc // { ${name} = lib.unique ((acc.${name} or []) ++ ps); }
      ) {} configuredEntries;
      allPkgNames = builtins.attrNames pragmasByName;

      # Baseline: pragmas present in *every* pkg-name's union.
      baseline =
        if allPkgNames == [] then []
        else
          let firstPkg = builtins.head allPkgNames;
          in lib.filter
               (p: lib.all (n: lib.elem p pragmasByName.${n}) allPkgNames)
               pragmasByName.${firstPkg};
      baselineSet = lib.listToAttrs (map (p: { name = p; value = true; }) baseline);

      baselineBlock =
        if baseline == [] then ""
        else "package *\n"
           + lib.concatMapStrings (p: "  " + p + "\n") baseline;

      perPkgBlocks = lib.concatMapStrings (name:
        let delta = lib.filter (p: !(baselineSet ? ${p})) pragmasByName.${name};
        in if delta == [] then ""
           else "package ${name}\n"
              + lib.concatMapStrings (p: "  " + p + "\n") delta
      ) allPkgNames;
    in baselineBlock + perPkgBlocks;

  # Cross GHCs only ship the unversioned bin (e.g.
  # `${ghc.targetPrefix}ghc`), not the `-X.Y.Z` suffixed one, so
  # for cross targets we have to drop the version suffix from the
  # name in `with-compiler:`.  For native we keep the version
  # suffix — cabal hashes the literal `with-compiler:` text into
  # `pkgHashCompilerId` (and therefore the UnitId), so leaving
  # the unversioned form here would fork UnitIds vs any
  # downstream user invoking `cabal v2-build` with the more
  # common `with-compiler: ghc-X.Y.Z` shape.
  withCompiler =
    if (ghc.targetPrefix or "") != ""
    then "${ghc.targetPrefix}ghc"
    else "ghc-${ghcPkgVersion}";
  # `extra-packages:` lists every package whose source build cabal
  # must consider:
  #   * the slice's own target package (so `:pkg:<pkg>:lib:<x>`
  #     targets resolve against the index)
  #   * every entry in `libConstraintPins` — without this, cabal's
  #     solver leaves transitive deps like `process` un-constrained
  #     because a cabal.project `constraints: process source` is
  #     silently ignored on packages that aren't goals in their own
  #     right.  Promoting them to extra-packages makes the solver
  #     consider the source-built copy and lets the `source`
  #     constraint kick in.
  # Skip the target from `extra-packages:` when we're going to put
  # it in `packages:` instead (local test / bench).  Listing it in
  # both is harmless but the local entry takes priority anyway.
  # Source-repo packages this slice's solver must see as
  # `source-repository-package`s rather than tarball
  # `extra-packages:` — both the slice's *own* pkg (when this is a
  # source-repo slice) and any source-repo package in the
  # transitive lib-dep closure.  Without the
  # `source-repository-package` block, cabal hashes the slicing
  # repo's tarball into `pkg-src-sha256` and computes a different
  # UnitId than plan-nix recorded; with it, cabal clones from the
  # `file://` URL into a minimal git repo and hashes the same
  # source bytes the project plan-nix saw, producing a matching
  # UnitId.  Each entry is `{ name; minRepo; }` where `minRepo` is
  # a tiny git repo wrapper (`git init -b minimal && git add . &&
  # git commit`) over the package's `src`.  Mirrors
  # `lib/call-cabal-project-to-nix.nix:209`.
  wrapAsMinimalRepo = name: pkgSrc:
    pkgs.runCommand "v2-source-repo-${name}"
      { nativeBuildInputs = [ pkgs.rsync pkgs.gitMinimal ];
        preferLocalBuild = true;
      }
      ''
        mkdir $out
        rsync -a --prune-empty-dirs --chmod=u+w "${pkgSrc}/" "$out/"
        cd $out
        git init -b minimal
        git add --force .
        GIT_COMMITTER_NAME='No One' GIT_COMMITTER_EMAIL= \
          git commit -m "Minimal Repo For Haskell.Nix" --author 'No One <>'
      '';
  sourceRepoEntries =
    let
      ownEntry = lib.optional isSourceRepoPkg
        { name = pkgName; minRepo = minimalSourceRepo; };
      depEntries = lib.concatMap (e:
        let p  = e.entry;
            pn = p.pkg-name or "";
            isSR = (p.pkg-src or {}).type or null == "source-repo";
            depPkg = hsPkgs.${pn} or null;
            depSrc = if depPkg != null then depPkg.src or null else null;
        in if pn != "" && pn != pkgName && isSR && depSrc != null
           then [{ name = pn; minRepo = wrapAsMinimalRepo pn depSrc; }]
           else []
      ) allDepClosure;
    in ownEntry
       ++ lib.foldl' (acc: e: if lib.any (a: a.name == e.name) acc
                              then acc else acc ++ [ e ]) [] depEntries;
  sourceRepoPkgNames = map (e: e.name) sourceRepoEntries;
  # `source-repository-package` blocks for every source-repo pkg
  # the slice's solver will see — see `sourceRepoEntries` above.
  sourceRepoBlocks = lib.concatMapStrings (e: ''
    source-repository-package
      type: git
      location: file://${e.minRepo}
      subdir: .
      tag: minimal
  '') sourceRepoEntries;
  # Skip from `extra-packages:` any pkg we've already declared
  # via a `source-repository-package` block — listing it as both
  # makes cabal's solver consider the remote-tarball candidate
  # too, which would compute a *different* UnitId and diverge
  # from plan-nix.
  extraPackagesEntries =
    lib.optional (!isLocalTestOrBench && !isSourceRepoPkg) "${pkgName} ==${pkgVersion}"
    ++ map (e: "${e.name} ==${e.version}")
         (lib.filter (e: !(lib.elem e.name sourceRepoPkgNames))
           libConstraintPins);
  extraPackagesLine =
    if extraPackagesEntries == [] then ""
    else "extra-packages: ${lib.concatStringsSep ", " extraPackagesEntries}\n";
  packagesLine =
    lib.optionalString isLocalTestOrBench
      "packages: ./src/${pkgName}-${pkgVersion}\n";
  cabalProject = ''
    with-compiler: ${withCompiler}
    active-repositories: hackage.haskell-nix
    ${packagesLine}${sourceRepoBlocks}${extraPackagesLine}${allowNewerBlock}${allowBootLibBlock}${projectConfigPragmas}${extraProject}${allFlagBlocks}${allGhcOptionsBlocks}${allConfigureOptionsBlocks}${allDocBlocks}${extraIncludeAndLibDirs}${depConstraints}'';

  # X-revised .cabal as a /nix/store path (or null if no override).
  # Carried on every `transitiveTarballs` entry so the slicing repo's
  # 00-index.tar.gz can stage the right .cabal per package without
  # extracting anything from the tarball when an override exists.
  thisCabalFile =
    if cabalFile != null
    then builtins.toFile "${pkgName}.cabal" cabalFile
    else null;
  thisEntry = { name = pkgName; version = pkgVersion; tarball = pkgTarball; cabalFile = thisCabalFile; };
  # `transitiveTarballs` is `depTarballsDeduped`'s values (which
  # already includes `thisEntry` via `depTransitives`).  Crucially
  # this MUST be the deduped list — `depTransitives` is the
  # concatenation of every direct dep's transitive list without
  # dedup, so the un-deduped form grows exponentially with depth
  # for a typical Haskell graph (every package shares `base`,
  # `bytestring`, `text`, ...).  `cardano-wallet`'s lib slice's
  # un-deduped transitive list materialises a single ~6.8 GiB
  # block during evaluation.  Deduping here keeps it linear in
  # the unique closure size (~hundreds of entries for typical
  # projects).
  transitiveTarballs = lib.attrValues depTarballsDeduped;

  # On targets that need a wrapped ghc (currently just windows
  # cross, where TH compiles route through the wineIservWrapper),
  # `templateHaskell.wrapGhc` is set by the host-platform overlay.
  # Apply it to the ghc we already have so cabal still sees the
  # same compiler-id (UnitId hash unaffected) but every compile
  # silently picks up `-fexternal-interpreter -pgmi <wrapper>`.
  sliceGhc =
    if templateHaskell != null && templateHaskell.wrapGhc or null != null
    then templateHaskell.wrapGhc ghc
    else ghc;

  # preBuild snippet shared between the real slice and
  # `checkAgainstPlan`: extract the package source for local
  # test / bench targets, then emit `cabal.project`.
  slicePreBuild = ''
    ${lib.optionalString isLocalTestOrBench ''
      # Extract the package source so `packages:` can reference
      # it as a local directory (cabal-7127 requires test / bench
      # targets to be local to the project).
      mkdir -p src
      tar -xzf ${pkgTarball} -C src
    ''}
    cat <<'EOF' > cabal.project
    ${cabalProject}EOF
  '';

  baseSlice = buildCabalStoreSlice {
    # Match v1's `comp-builder.nix:268`-style
    # `<pkg>-<ctype>-<cname>-<version>` so callers (tests, store
    # paths) can move from builderVersion 1 → 2 without seeing
    # different derivation names for the same component.
    name = "${pkgName}-${componentKindLabel}-${componentName}-${pkgVersion}";
    inherit depSlices;
    ghc = sliceGhc;
    localRepo = slicingRepo;
    preBuild = slicePreBuild;
    target = targetSelector;
    # When this slice targets a sublib (e.g.
    # `:pkg:io-classes:lib:strict-stm`), tell our patched
    # cabal-install to add `${cname}` as an extra reachability seed
    # for `${pkgName}` — otherwise the `prune-unreachable-sublibs`
    # patch drops the targeted sublib (the main lib doesn't
    # `build-depends:` it) and cabal raises `[Cabal-7127]`.  We only
    # add the targeted sublib (not all of them), so unrelated sublibs
    # like `lib:testlib` are still pruned and their deps don't leak
    # into the install plan.
    extraSublibSeeds = extraSublibSeeds;
    inherit extraBuildInputs extraNativeBuildInputs withProgFlags
            allowedBuildToolPackages confLibraryDirs;
    # Things that flow into downstream slices' build envs through
    # stdenv's propagation chain.  Library deps don't appear here
    # because in v2 they're the dep slices themselves, already in
    # `buildInputs`.
    #
    # Propagating `libs` here would propagate them to *every*
    # consumer — including consumers on a different stdenv
    # (the v2 dev shell on aarch64-darwin sticks Linux cross
    # slices into `inputsFrom`).  Stdenv would then take a
    # Linux-target `pkgs.liburing` and try to build it for
    # Darwin.  Limit `libs` propagation to Windows, where
    # `wineIservWrapper` walks `pkgsHostTarget` for DLL
    # discovery (matching v1).  For non-Windows targets each
    # slice gathers transitive sysLibs locally via
    # `transitiveDepLibs`, kept inside its own
    # `extraBuildInputs`.
    propagated = haskellLib.checkUnique
      "${ghc.targetPrefix}${pkgName}-${pkgVersion}-${componentKindLabel}-${componentName} propagatedBuildInputs" (
           haskellLib.uniqueWithName frameworks
        ++ haskellLib.uniqueWithName pkgconfig
        ++ lib.optionals stdenv.hostPlatform.isWindows
          (haskellLib.uniqueWithName libs));
    # The slice's `cabal v2-build` should only ever build the
    # target package (any number of components — lib + sublibs +
    # exe, etc.) plus, on cross, the transitive build-tools that
    # cabal's solver always plans for `build-tool-depends:` goals.
    # Any OTHER package appearing in cabal's plan means a lib dep
    # we composed into the starting store ended up rebuilt.
    expectedPackage = pkgName;
    # The slice must produce its own component's plan-id.
    # `package.identifier.unit-id` is set by
    # `modules/install-plan/planned.nix` to the plan-id of the
    # specific install-plan entry this slice was built from.
    #
    # Skip the unit-id check when plan-nix recorded `style: "local"`
    # for this component: local packages are listed in cabal.project's
    # `packages:` block, so plan-nix's unit-id was hashed with a
    # `pkg-src: { type: local, ... }`.  The slice serves the same
    # source through the slicing repo and `extra-packages:`, which
    # cabal treats as `style: "global"` / `pkg-src: { type: repo-tar }`
    # — same source bytes, different `pkgHashSourceHash` inputs,
    # different unit-id.  The unit-id check would always fail; the
    # plan diff at `.checkAgainstPlan` is the right tool for spotting
    # real divergence here.
    expectedUnitId =
      let uid = package.identifier.unit-id or null;
          entry = if uid == null then null else planJsonByPlanId.${uid} or null;
          style = if entry == null then null else entry.style or null;
          # Custom-build packages share a single plan-nix entry across
          # every component (lib + sublib + exe + ...).  The slice's
          # cabal-install splits them back into per-component units
          # with distinct UnitIds, so the slice's UnitId can never
          # equal the plan-nix `id`.  Plan-nix marks the shared entry
          # by leaving `component-name` unset / null.
          isCustomBuild = entry != null && (entry.component-name or null) == null;
      in if isCross || style == "local" || isCustomBuild then null else uid;
    # Plan-json entry for this slice's expected unit-id, written to
    # disk so the unit-id-mismatch diagnostic can diff what plan-nix
    # said this component should look like against what cabal actually
    # built (the matching entry in the slice's
    # `dist-newstyle/cache/plan.json`).
    expectedPlanEntries =
      let uid = package.identifier.unit-id or null;
      in if uid == null || (planJsonByPlanId.${uid} or null) == null
         then null
         else { ${uid} = planJsonByPlanId.${uid}; };
    passthru = {
      inherit pkgTarball transitiveTarballs;
      # Mirror v1's `.config` passthru so downstream code that walks
      # the dep graph via flatLibDepends (see shell-for-v2.nix,
      # haskellLib.flatLibDepends) can traverse v2 components the
      # same way as v1.
      config = component;
      # `lib/cover.nix` (HPC coverage report) reads `srcSubDirPath`
      # off each mix library to know where the .hs sources live for
      # `hpc markup --srcdir=...`.  v1 plumbs this from `cleanSrc`;
      # mirror the shape here so cover.nix works against v2 slices.
      srcSubDir = (haskellLib.rootAndSubDir src).subDir;
      srcSubDirPath = let r = haskellLib.rootAndSubDir src; in r.root + r.subDir;
      identifier = package.identifier // {
        component-id = "${pkgName}:${componentId.ctype}:${componentId.cname}";
        component-name = componentId.cname;
        component-type = componentId.ctype;
      };
      # Expose the resolved `build-tool-depends` entries too, again to
      # match v1's passthru shape.  Shells (particularly the v2 shell
      # with `allToolDeps`) gather these to put build tools on PATH
      # before the user runs `cabal build`.
      executableToolDepends = extraNativeBuildInputs;
      # Every host-platform lib the slice was linked against.
      # `lib/check.nix`'s v2 branch adds these to its
      # `nativeBuildInputs` so test runners (wineTestWrapper for
      # windows, etc.) can find the runtime DLLs / dylibs / .so
      # files the exe imports.  This mirrors v1's automatic
      # `propagatedBuildInputs` chain — v2 slices don't ride
      # stdenv's propagation since dep slices flow as a
      # `DEP_SLICES` env var rather than as buildInputs, so we
      # surface the same set explicitly.
      runtimeLibs = extraBuildInputs;
    };
  };

  # When `coverage: True` is set, cabal stores per-package HPC
  # artifacts under `$out/store/<ghc>/<unit-id>/lib/extra-compilation-artifacts/hpc/`
  # (mangled cabal unit-id name, e.g. `pkg-0.1.0.0-f77c657f`).
  # The .mix file *content* is derived from source, not unit-id —
  # so a sibling test slice that rebuilds the lib inplace
  # (UnitId `<pkg>-<ver>-inplace`) produces byte-identical .mix
  # files.  Surface the lib slice's mixes under
  # `$out/share/hpc/<way>/mix/<pkg>-<ver>-inplace/` so HPC matches
  # the path against `<pkg>-<ver>-inplace/<Module>` references in
  # tix files dropped by inplace-built test exes.  Tix files don't
  # land here (they're produced at test-run time by `lib/check.nix`).
  hpcCopyForLibrary = lib.optionalString isLibrary ''
    for src_hpc in $out/store/ghc-*/*/lib/extra-compilation-artifacts/hpc; do
      [ -d "$src_hpc" ] || continue
      for way_dir in "$src_hpc"/*/; do
        [ -d "$way_dir" ] || continue
        way=$(basename "$way_dir")
        if [ -d "$way_dir/mix" ]; then
          mkdir -p "$out/share/hpc/$way/mix/${pkgName}-${pkgVersion}-inplace"
          for unit_dir in "$way_dir/mix"/*/; do
            [ -d "$unit_dir" ] || continue
            cp -r "$unit_dir"/. "$out/share/hpc/$way/mix/${pkgName}-${pkgVersion}-inplace/"
          done
        fi
      done
    done
  '';

  # Trim cabal's `dist-newstyle/` from the slice's $out at the
  # end of installPhase: the only consumer that needs it during
  # this build is `kindSpecificInstallPhase`'s test/bench find
  # (which runs above before this trim).  Downstream slices /
  # callers don't read the slice's `$out/dist-newstyle/`, so we
  # can drop the source-tarball / build-tree bulk to keep slice
  # outputs small.  Lift `cache/plan.json` to `$out/plan.json`
  # so it stays available for human debugging.
  trimDistNewstyle = ''
    if [ -d $out/dist-newstyle ]; then
      if [ -f $out/dist-newstyle/cache/plan.json ]; then
        cp $out/dist-newstyle/cache/plan.json $out/plan.json
      fi
      chmod -R u+w $out/dist-newstyle 2>/dev/null || true
      rm -rf $out/dist-newstyle
    fi
  '';

  # Per-kind installPhase tail: surface binaries for exe / test /
  # bench, and run the test as part of the build.  The final binary
  # lands at `$out/bin/${exeName}` (platform-specific extension —
  # `.exe` on native Windows, `.wasm` for wasm, etc.).
  kindSpecificInstallPhase =
    if isLibrary then hpcCopyForLibrary + trimDistNewstyle
    else if useTarball then ''

      mkdir -p $out/bin
      # `\( -type f -o -type l \)` because cabal may say "Up to date"
      # and skip rebuilding when a depSlice's lndir composition already
      # supplied the exe — in which case `$out/store/.../bin/<exe>` is
      # a symlink into the dep slice, not a real file.
      bin=$(find $out/store \( -type f -o -type l \) -path "*/bin/${exeName}" | head -n1)
      if [ -z "$bin" ]; then
        # TODO: this currently fires when cabal short-circuits on
        # "Up to date" without materialising the exe in $out/store,
        # which can happen when a transitive slice already supplied
        # the unit and our composition didn't carry the bin/.  Emit
        # a non-functional placeholder so the slice still produces
        # an output (lets the dev shell open while we investigate).
        echo "WARN: ${componentKindLabel} binary ${exeName} not found in $out/store" >&2
        echo "      emitting placeholder so this slice still produces an output" >&2
        printf '#!/bin/sh\necho "%s: placeholder — slice did not materialise the exe" >&2\nexit 1\n' \
          "${exeName}" > $out/bin/${exeName}
        chmod +x $out/bin/${exeName}
      else
        cp "$bin" $out/bin/${exeName}
        chmod +x $out/bin/${exeName}
      fi
      # `build-cabal-slice.nix`'s installPhase already cleared
      # dep-slice content out of $out/store (keeping only the
      # expected unit's dir + conf).  No further cleanup needed
      # here — the bin we just copied came from
      # `$out/store/<uid>/bin/${exeName}`, which survived the
      # cleanup as part of the expected unit's dir.
      ${trimDistNewstyle}
    '' else ''

      mkdir -p $out/bin
      bin=$(find $out/dist-newstyle \( -type f -o -type l \) -name '${exeName}' | head -n1)
      if [ -z "$bin" ]; then
        echo "FAIL: ${componentKindLabel} binary ${exeName} not found in $out/dist-newstyle" >&2
        find $out/dist-newstyle \( -type f -o -type l \) >&2
        exit 1
      fi
      cp "$bin" $out/bin/${exeName}
      chmod +x $out/bin/${exeName}
      ${trimDistNewstyle}
    '';

  # v2 test slices only *build* the test binary — actually running
  # it is left to `lib/check.nix` (via `haskellLib.check`), matching
  # v1's separation of build-time and check-time.  This avoids
  # paying the test run cost when the caller only needs the
  # binary built (e.g. for offline inspection or alternate runners).

  slice = baseSlice.overrideAttrs (old: {
    installPhase = old.installPhase + kindSpecificInstallPhase;
  });

  # Sibling drv that re-runs only the slice's *planning* step (cabal
  # v2-build --dry-run) and dumps a per-package diff between the
  # entry plan-nix recorded for that package and the entry cabal
  # actually planned in this slice.  Useful when a slice fails the
  # unit-id check (or plans an unexpected package) — the diff makes
  # the diverging field visible (compiler-id, flags, dep unit-ids,
  # ...).  Inspect with:
  #   nix-build -A project.hsPkgs.<pkg>.components.library.checkAgainstPlan
  # then read $out/diff-report.txt.  Output also includes the slice's
  # plan.json and a copy of plan-nix.json so they can be diffed
  # outside of nix.  Plan-nix.json is plumbed in as a /nix/store path
  # (via `pkgsBuildBuild.writeText`) so it's a dep of this drv only,
  # not of the slice itself.
  planNixJsonFile = pkgs.buildPackages.writeText "plan-nix.json"
                      (builtins.toJSON planJson);
  checkAgainstPlan = buildCabalStoreSlice {
    name = "check-${pkgName}-${componentKindLabel}-${componentName}-${pkgVersion}";
    inherit depSlices;
    ghc = sliceGhc;
    localRepo = slicingRepo;
    preBuild = slicePreBuild;
    target = targetSelector;
    extraSublibSeeds = extraSublibSeeds;
    inherit extraBuildInputs extraNativeBuildInputs withProgFlags
            allowedBuildToolPackages confLibraryDirs;
    dryRunOnly = true;
    inherit planNixJsonFile;
  };

  # cabal-install records `documentation: True` (from cabal.project)
  # by setting `elabBuildHaddocks = True` on the elaborated install
  # plan and ALSO by adding `--ghc-option=-haddock` to the unit's
  # `configure-args` (so GHC keeps haddock comments in `.hi` files).
  # We use the `--ghc-option=-haddock` plan-json signal as evidence
  # that documentation was enabled at project-eval time — it's the
  # only haddock-related thing plan-nix surfaces in `configure-args`,
  # since the booleans (`elabBuildHaddocks`, the haddock-html /
  # haddock-hscolour / haddock-internal / ... family) live on the
  # ElaboratedConfiguredPackage and don't show up as configure-args.
  # When we see `-haddock` for a unit, we emit `documentation: True`
  # for that package in the slice's cabal.project (see
  # `documentationBlockFor` below) so the slice's elaboration sets
  # the same haddock booleans plan-nix did and the UnitIds match.
  docEnabled =
    let uid   = package.identifier.unit-id or null;
        entry = if uid == null then null else planJsonByPlanId.${uid} or null;
        args  = if entry == null then [] else (entry.configure-args or []);
    in lib.elem "--ghc-option=-haddock" args;

  # Sibling slice that runs `cabal v2-haddock` and surfaces
  # haddock html in cabal's native unit-dir layout
  # (`$out/store/<ghc>/<unit-id>/share/doc/html/`).  Keeping
  # the cabal-store layout means the doc slice can be `lndir`'d
  # into `~/.cabal/store/` like any other slice.  Built only on
  # attribute access; the dry-run check below catches the case
  # where `documentation` propagation is incomplete.  Each unit
  # in the doc slice's `$out/store` carries only that unit's
  # own target's html; dep haddocks come from the corresponding
  # deps' own `.doc` slices propagated via `depSlices` — but
  # only for deps whose own plan-nix entry has documentation
  # enabled.  Mixed projects (some packages with docs, others
  # without) keep working because non-doc deps come in as plain
  # slices.
  docSlice = buildCabalStoreSlice {
    name = "${pkgName}-${componentKindLabel}-${componentName}-${pkgVersion}-doc";
    # Compose the main slice in plus, for each lib dep that
    # has documentation enabled in plan-nix, that dep's `.doc`
    # slice — so cabal v2-haddock finds the haddock-interfaces
    # it needs for `--read-interface=` (cross-package
    # hyperlinks) and doesn't try to rebuild the dep with
    # `--enable-documentation`.  Deps without docs keep coming
    # in as plain slices so mixed projects work.
    depSlices = depSlices ++ [ slice ]
                ++ map (d: d.doc) (lib.filter
                     (d: (d.passthru.docEnabled or false))
                     depSlices);
    ghc = sliceGhc;
    localRepo = slicingRepo;
    preBuild = slicePreBuild;
    target = targetSelector;
    extraSublibSeeds = extraSublibSeeds;
    inherit extraBuildInputs extraNativeBuildInputs withProgFlags
            allowedBuildToolPackages confLibraryDirs;
    doHaddock = true;
    # The build-cabal-slice dry-run check fails when cabal plans
    # to build anything other than `expectedPackage` — that's
    # exactly the diagnostic we want for `.doc`: if dep doc
    # slices' UnitIds don't match what cabal v2-haddock would
    # compute (i.e. the project hasn't enabled documentation
    # everywhere), the user gets a clear "package X needs
    # rebuild" error instead of a silent slow build.
    expectedPackage = pkgName;
  };

  # Composed dep store for this component — slices it builds on top
  # of, *not* the slice itself.  Useful as a starting point for
  # downstream `cabal v2-build` invocations, or for inspection
  # (`nix-build -A foo.components.library.store`).  `depSlices` is
  # only the *direct* set; `composeStore` follows each entry's
  # `$out/nix-support/transitive-deps` file for the closure.
  store = composeStore {
    name = "store-${ghc.targetPrefix or ""}${pkgName}-${componentKindLabel}-${componentName}-${pkgVersion}";
    slices = depSlices;
  };

  # Mirror v1's passthru/meta for exe / test / bench slices:
  #   * `exePath` — absolute path to the produced binary
  #   * `exeName` — filename (with platform extension)
  #   * `meta.mainProgram` — makes `nix run` target the right binary
  # See comp-builder.nix:453, 546-547, 576 for the v1 equivalents.
  isExeLike = ctype == "exe" || ctype == "test" || ctype == "bench";
  exePathAttrs = lib.optionalAttrs isExeLike {
    inherit exeName;
    exePath = "${slice}/bin/${exeName}";
  };

  # v1 exposed `.profiled` as an overlay rebuild with
  # `enableLibraryProfiling = true`.  v2 reads configure-args from
  # plan.json, so an overlay would emit toggles that plan-nix
  # doesn't know about and the slice's UnitId would diverge.
  # Express profiling in `cabal.project` instead; see
  # `docs/dev/profiling.md` for the migration recipe.  Setting
  # this at both the top level and inside `passthru` is needed
  # because mkDerivation only lifts `passthru` → top level at
  # derivation creation time, so post-hoc `// { passthru = ... }`
  # doesn't re-lift.
  profiledThrow = throw ''
    v2 slices do not provide `.profiled`.  Enable profiling
    by adding to cabal.project (or `cabalProjectLocal`):

        package ${pkgName}
          profiling: True
          library-profiling: True

    (or `package *` for project-wide).  See
    `docs/dev/profiling.md` for the rationale.
  '';

  # `.doc` is the sibling haddock slice when the package's
  # plan-nix entry already includes `--ghc-option=-haddock`;
  # otherwise it throws with a migration recipe.  We don't
  # synthesise `.haddockDir` — local-package UnitIds in
  # plan-nix (`<pkg>-<ver>-inplace…`) don't equal cabal's
  # mangled-hash form in the slice's `$out/store/`, so there's
  # no eval-time-stable html path; callers should `find` it
  # under `slice.doc`.
  docAttrs = lib.optionalAttrs isLibrary {
    doc = if docEnabled then docSlice else throw ''
      v2 slices only expose `.doc` when haddock is part of the
      build's UnitId-determining inputs.  Add to cabal.project
      (or `cabalProjectLocal`):

          package ${pkgName}
            documentation: True

      (or `package *` for project-wide).  See
      `docs/dev/haddock.md` for the rationale.
    '';
    # Surfaced via passthru too so doc slices can branch in
    # `depSlices` between a dep's `.doc` (when it has one) and
    # the dep's regular slice (when it doesn't).
    inherit docEnabled;
  };

  decorate = base: base // exePathAttrs // docAttrs // {
      inherit checkAgainstPlan;
      profiled = profiledThrow;
      passthru = base.passthru // {
        inherit checkAgainstPlan;
        isSlice = true;
        profiled = profiledThrow;
      } // exePathAttrs // docAttrs;
      meta = (base.meta or {}) // lib.optionalAttrs isExeLike {
        mainProgram = exeName;
      };
      inherit store;
      # Noop `override` for v2 slices.  v1 derivations are
      # overridable functions; v2 slices are concrete derivations,
      # so we ignore the override args and return the same
      # decorated slice — and crucially preserve `override` on the
      # result so back-to-back `drv.override {...}.override {...}`
      # calls (as in the iserv-proxy-interpreter / -prof pair) work.
      override = _: decorate base;
    };
in decorate slice
