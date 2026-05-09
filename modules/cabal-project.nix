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
    cabalProjectLocal = mkOption {
      type = nullOr lines;
      default = readIfExists config.evalSrc "${config.cabalProjectFileName}.local";
    };
    cabalProjectFreeze = mkOption {
      type = nullOr lines;
      default = readIfExists config.evalSrc "${config.cabalProjectFileName}.freeze";
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
    # Musl host: every executable should be statically linked.  v1
    # achieves this in `builder/comp-builder.nix:384` by adding
    # `--ghc-option=-optl=-static` to the per-component
    # configureFlags at build time, which doesn't reach plan-to-nix
    # — plan.json keeps `--disable-executable-static` and v1 just
    # ignores the misalignment.  Under v2, the slice mirrors
    # plan.json's configure-args exactly, so it links dynamically
    # too and the c-ffi musl test fails its static-link assertion.
    # Inject `executable-static: True` here so plan-to-nix records
    # `--enable-executable-static` from the start; v2's slice picks
    # it up via `comp-v2-builder.nix:projectConfigPragmas`, and v1
    # is unaffected (the post-plan ghc-option still fires, harmless).
    (lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
      cabalProjectLocal = lib.mkBefore ''
        -- Auto-injected by haskell.nix: musl targets always link
        -- executables statically.  See `modules/cabal-project.nix`
        -- for the rationale.
        executable-static: True
      '';
    })
    (lib.mkIf config.useLocalGhcLib (
    let
      ghc = (config.compilerSelection pkgs.buildPackages).${config.compiler-nix-name};
      ghcSrc = (pkgs.buildPackages.symlinkJoin {
        name = ghc.name + "-full-src";
        paths = [ ghc.configured-src ghc.generated ];
      }) + "/compiler";
      ghcMinRepoUrl = "file://${ghcSrc}";
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
      # `lib/call-cabal-project-to-nix.nix:209` and the slice's
      # `comp-v2-builder.nix` does the same — both wrappers produce
      # deterministic content (same rsync + git init + commit), so
      # cabal's `pkg-src-sha256` matches between plan-nix and slice.
      #
      # Why source-repository-package and not `packages:`:
      #   * `packages:` makes cabal treat the package as *inplace* —
      #     v2-build registers `<pkg>-<ver>-inplace` in dist-newstyle
      #     but doesn't copy anything to the cabal-store layout, so
      #     the slice's `$out/store/<ghc>/<unit-id>/` ends up empty
      #     and consumers can't find the unit.
      #   * source-repository-package takes the regular reinstallable
      #     path: cabal hashes the wrapped repo's content into
      #     `pkg-src-sha256`, builds the package, and *installs* it
      #     to the cabal store like any other reinstallable dep.
      #
      # `allow-boot-library-installs: True` is needed at project
      # level too — plan-to-nix's cabal-install would otherwise
      # reject ghc's source instance.  The slice picks it up via
      # `comp-v2-builder.nix:allowBootLibBlock` independently.
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
      };
    }
    ))
  ];
}
