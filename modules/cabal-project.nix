{ lib, config, pkgs, haskellLib, ... }:
with lib;
with types;
let readIfExists = src: fileName:
      # Using origSrcSubDir bypasses any cleanSourceWith.
      # `lookForCabalProject` allows us to avoid looking in source from hackage
      # for cabal.project files.  It is set in `modules/hackage-project.nix`.
      let origSrcDir = src.origSrcSubDir or src;
      in
        if (src.lookForCabalProject or true) && builtins.elem ((__readDir origSrcDir)."${fileName}" or "") ["regular" "symlink"]
          then __readFile (origSrcDir + "/${fileName}")
          else null;
in {
  _file = "haskell.nix/modules/cabal-project.nix";
  options = {
    # Used by callCabalProjectToNix
    compiler-nix-name = mkOption {
      type = str;
      description = "The name of the ghc compiler to use eg. \"ghc884\"";
      # Map short version names to the latest GHC version.
      apply = name: pkgs.haskell-nix.resolve-compiler-name name;
    };
    compilerSelection = mkOption {
      type = unspecified;
      default = p: builtins.mapAttrs (_: x: x.override { ghcEvalPackages = config.evalPackages; }) p.haskell-nix.compiler;
      description = "Use GHC from pkgs.haskell instead of pkgs.haskell-nix";
    };
    index-state = mkOption {
      type = nullOr str;
      default = null;
      description = "Hackage index-state, eg. \"2019-10-10T00:00:00Z\"";
    };
    index-sha256 = mkOption {
      type = nullOr str;
      default = null;
      description = "The hash of the truncated hackage index-state";
    };
    plan-sha256 = mkOption {
      type = nullOr str;
      default = null;
      description = "The hash of the plan-to-nix output (makes the plan-to-nix step a fixed output derivation)";
    };
    materialized = mkOption {
      type = nullOr (either path package);
      default = null;
      description = "Location of a materialized copy of the nix files";
    };
    checkMaterialization = mkOption {
      type = nullOr bool;
      default = null;
      description = "If true the nix files will be generated used to check plan-sha256 and material";
    };
    cabalProjectFileName = mkOption {
      type = str;
      default = "cabal.project";
    };
    cabalProject = mkOption {
      type = nullOr lines;
      default = readIfExists config.evalSrc config.cabalProjectFileName;
    };
    # `cabalProjectLocal` / `cabalProjectFreeze` no longer
    # auto-load `cabal.project.local` / `cabal.project.freeze` from
    # the project source.  Projects that want that behaviour set
    # the option explicitly:
    #
    #   cabalProjectLocal = builtins.readFile ./cabal.project.local;
    #
    # Plain `lines` (not `nullOr lines`) so `lib.mkBefore` directives
    # from platform-conditional defaults below merge cleanly with
    # whatever the user passes.
    cabalProjectLocal = mkOption {
      type = lines;
      default = "";
    };
    cabalProjectFreeze = mkOption {
      type = lines;
      default = "";
    };
    ghc = mkOption {
      type = nullOr package;
      default = null;
      description = "Deprecated in favour of `compiler-nix-name`";
    };
    ghcOverride = mkOption {
      type = nullOr package;
      default = null;
      description = "Used when we need to set ghc explicitly during bootstrapping";
    };
    # The defaults for `nix-tools` and `cabal-install` are in `call-cabal-project-to-nix.nix`
    # to make sure it is not evaluated too strictly (which would lead to infinite recursion).
    nix-tools = mkOption {
      type = nullOr package;
      default = null;
      description = "nix-tools to use when converting the `plan.json` to nix";
    };
    configureArgs = mkOption {
      type = nullOr (separatedString " ");
      default = "";
      description = ''
        Extra arguments to pass to `cabal v2-configure`.
        `--enable-tests --enable-benchmarks` are included by default.
        If the tests and benchmarks are not needed and they
        cause the wrong plan to be chosen, then we can use
        `configureArgs = "--disable-tests --disable-benchmarks";`
      '';
    };
    sha256map = mkOption {
      type = nullOr (attrsOf (either str (attrsOf str)));
      # Default needed for haskell-language-server 1.10
      default."https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ=";
      description = ''
        An alternative to adding `--sha256` comments into the
        cabal.project file:
          sha256map =
            { "https://github.com/jgm/pandoc-citeproc"."0.17"
              = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; };
      '';
    };
    inputMap = mkOption {
      type = nullOr attrs;
      default = {};
      description = ''
        Specifies the contents of urls in the cabal.project file.
        The `.rev` attribute is checked against the `tag` for `source-repository-packages`.
        # FIXME is the following still relevant?
        For `revision` blocks the `inputMap.<url>` will be used and
        they `.tar.gz` for the `packages` used will also be looked up
        in the `inputMap`.
      '';
    };
    extra-hackage-tarballs = mkOption {
      type = nullOr attrs;
      default = {};
    };
    source-repo-override = mkOption {
      type = attrsOf (functionTo attrs);
      default = {};
    };
    supportHpack = mkOption {
      type = bool;
      default = false;
    };

    # Used by mkCabalProjectPkgSet
    pkg-def-extras = mkOption {
      type = nullOr (listOf unspecified);
      default = [];
    };
    modules = mkOption {
      type = nullOr (listOf unspecified);
      default = [];
    };
    extra-hackages = mkOption {
      type = nullOr (listOf unspecified);
      default = [];
    };
    prebuilt-depends = mkOption {
      type = listOf package;
      default = [];
      description = ''
        pre-built (perhaps proprietary) Haskell packages to make available as dependencies

        See Note [prebuilt dependencies] for more details
      '';
    };
  };
  config = lib.mkMerge [
    # Musl host: every executable should be statically linked.
    # comp-builder achieves this at build time by adding
    # `--ghc-option=-optl=-static` to the per-component
    # configureFlags, which doesn't reach plan-to-nix — plan.json
    # keeps `--disable-executable-static` and the misalignment is
    # ignored.  Surface the toggle in cabal.project so plan-to-nix
    # records `--enable-executable-static` for every unit; the
    # actual build is unchanged.  Set inside `package *` because
    # cabal only propagates `executable-static` per-component when
    # the directive lives in that block.
    (lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
      cabalProjectLocal = lib.mkBefore ''
        package *
          executable-static: True
      '';
    })
    # x86_64-darwin host: enable `library-for-ghci` so cabal emits a
    # merged `HS<unit>.o` per unit alongside the `.dylib`.
    # comp-builder already passes `--enable-library-for-ghci` for
    # !ghcjs / !wasm / !android (which on darwin is always),
    # so this matches the on-disk artefacts to what plan-to-nix
    # records.  Helps TH-eval via `ghc-iserv-dyn` find a merged
    # `.o` to load directly, bypassing dyld dylib weirdness under
    # Rosetta (Hydra builds x86_64-darwin on aarch64-darwin
    # hardware).  Scoped to x86_64 — aarch64-darwin runs natively.
    (lib.mkIf (pkgs.stdenv.hostPlatform.isDarwin
            && pkgs.stdenv.hostPlatform.isx86_64) {
      cabalProjectLocal = lib.mkBefore ''
        package *
          library-for-ghci: True
      '';
    })
    # Android host: link every exe statically so qemu-user can run
    # it on the build host.  A dynamic Android binary references
    # `/system/bin/linker[64]` at runtime; qemu-arm fails with
    # `Could not open '/system/bin/linker': No such file or
    # directory` because the build host doesn't ship one.
    # `lib/check.nix` papers over this by re-overriding the test
    # exe with `setupBuildFlags = ["--ghc-option=-optl-static"]`;
    # surfacing the same flags at the project level here keeps
    # plan-to-nix's recorded configure-args matching the artefact.
    # `-optl-static` makes the exe self-contained, `-optl-ldl`
    # pulls in libdl that GHC's RTS still references even under
    # static linking, and on aarch32 `-optl-no-pie` disables PIE
    # so the static link doesn't trip `dynamic STT_GNU_IFUNC`
    # relocation errors.
    (lib.mkIf pkgs.stdenv.hostPlatform.isAndroid {
      cabalProjectLocal = lib.mkBefore ''
        package *
          ghc-options: -optl-static -optl-ldl${
            lib.optionalString pkgs.stdenv.hostPlatform.isAarch32 " -optl-no-pie"
          }
      '';
    })
    # wasm 9.12+: real wasm-ghc reports `target RTS linker only
    # supports shared libraries: YES` and no `Support shared
    # libraries` field at all.  cabal interprets the absence of
    # `Support shared libraries` as "no shared support" and
    # records `--disable-shared` in plan-nix, but TH-eval / dyld
    # on wasm 9.12+ requires `.so` files for every transitively
    # reachable lib (the RTS linker only loads shared libs).
    # Forcing `shared: True` at the project level keeps plan-nix's
    # recorded UnitIds matching what a downstream cabal v2-build
    # against the real wasm GHC would compute.
    (lib.mkIf (
      let ghc = (config.compilerSelection pkgs.buildPackages).${config.compiler-nix-name};
      in pkgs.stdenv.hostPlatform.isWasm
         && builtins.compareVersions ghc.version "9.12" >= 0
    ) {
      cabalProjectLocal = lib.mkBefore ''
        package *
          shared: True
      '';
    })
    (lib.mkIf config.useLocalGhcLib (
    let
      ghc = (config.compilerSelection pkgs.buildPackages).${config.compiler-nix-name};
      ghcFullSrc = pkgs.buildPackages.symlinkJoin {
        name = ghc.name + "-full-src";
        paths = [ ghc.configured-src ghc.generated ];
      };
      ghcSrc = ghcFullSrc + "/compiler";
      ghcMinRepoUrl = "file://${ghcSrc}";
      # `lib:ghc`'s `build-tool-depends` (when the `build-tool-depends`
      # flag is on) names `genprimopcode` and `deriveConstants`, which
      # live in the GHC source tree (not on hackage).  Expose them as
      # reinstallable source-repository-packages too, so cabal can
      # satisfy those exe build-tool goals from the same tree.
      genprimopcodeSrc = ghcFullSrc + "/utils/genprimopcode";
      deriveConstantsSrc = ghcFullSrc + "/utils/deriveConstants";
      genprimopcodeUrl = "file://${genprimopcodeSrc}";
      deriveConstantsUrl = "file://${deriveConstantsSrc}";
    in {
      # When `useLocalGhcLib = true`, expose the GHC compiler tree
      # (configured + generated) as a `source-repository-package` in
      # the project's cabal.project (via cabalProjectLocal), pointing
      # at the `compiler/` subdir of `(configured-src + generated)`.
      #
      # `inputMap` short-circuits haskell.nix's source-repo fetch so
      # we don't go through `builtins.fetchGit` (which fails in pure
      # eval mode without a sha256).  haskell.nix then re-wraps
      # `inputMap.<url>` in its own minimal git repo at
      # `lib/call-cabal-project-to-nix.nix` -- the wrapper produces
      # deterministic content (same rsync + git init + commit), so
      # cabal's `pkg-src-sha256` is stable across evaluations.
      #
      # Why source-repository-package and not `packages:`:
      #   * `packages:` makes cabal treat the package as *inplace* --
      #     v2-build registers `<pkg>-<ver>-inplace` in dist-newstyle
      #     but doesn't copy anything to the cabal-store layout.
      #   * source-repository-package takes the regular reinstallable
      #     path: cabal hashes the wrapped repo's content into
      #     `pkg-src-sha256`, builds the package, and installs it
      #     like any other reinstallable dep.
      #
      # `allow-boot-library-installs: True` is needed at project
      # level so plan-to-nix's cabal-install doesn't reject ghc's
      # source instance.
      cabalProjectLocal = lib.mkBefore ''
        -- Added by `useLocalGhcLib = true`: expose the GHC compiler
        -- tree (configured + generated) as a source-repository-package
        -- so cabal treats `lib:ghc` like a regular reinstallable
        -- package (installed to the cabal-store, not inplace).
        source-repository-package
          type: git
          location: ${ghcMinRepoUrl}
          subdir: .
          tag: minimal
        source-repository-package
          type: git
          location: ${genprimopcodeUrl}
          subdir: .
          tag: minimal
        source-repository-package
          type: git
          location: ${deriveConstantsUrl}
          subdir: .
          tag: minimal
        allow-boot-library-installs: True
      '';
      # Key by `<url>/<ref>` (the first lookup form in
      # `lib/call-cabal-project-to-nix.nix:fetchPackageRepo`) so we
      # short-circuit the `.rev` check that the bare-`<url>` form
      # applies — the local source isn't a git derivation and has
      # no `.rev` attribute.  Strip string context from the key
      # because nix forbids attribute names that carry references
      # to store paths.
      inputMap = {
        ${builtins.unsafeDiscardStringContext "${ghcMinRepoUrl}/minimal"} = ghcSrc;
        ${builtins.unsafeDiscardStringContext "${genprimopcodeUrl}/minimal"} = genprimopcodeSrc;
        ${builtins.unsafeDiscardStringContext "${deriveConstantsUrl}/minimal"} = deriveConstantsSrc;
      };
    }
    ))
  ];
}
