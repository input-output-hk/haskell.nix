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
  imports = [ ./replace-hackage-tarball-urls.nix ];
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
      default = p: builtins.mapAttrs (_: x: x.evalWith.${config.evalSystem} or (x.override { evalSystem = config.evalSystem; })) p.haskell-nix.compiler;
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
    withBuildCompiler = mkOption {
      type = bool;
      default = false;
      description = ''
        Pass a *build*-platform dummy `ghc` + `ghc-pkg` to `make-install-plan`
        during plan-to-nix, via `--with-build-compiler` / `--with-build-hc-pkg`
        (the stable-haskell cabal fork's cross "stage" system,
        haskell/cabal#11179).

        The build dummy's `ghc --info` reports the build (native) platform and
        its `ghc-pkg dump --global` lists the build boot libraries (especially
        `ghc-internal`) as installed.  That satisfies a source project's
        `constraints: build:any.ghc-internal installed` and lets cabal resolve
        build-tool-depends (e.g. `hsc2hs`) in the build scope, reusing the
        installed build-stage `process`/`base` rather than rebuilding them —
        breaking cross build-tool cycles (e.g. the Windows
        `Win32 -> hsc2hs -> process -> Win32` cycle) without the full Hackage
        index.  Off by default; projects that don't set it get no
        `--with-build-compiler` (unchanged behaviour).
      '';
    };
    prepopulateHackageIndex = mkOption {
      type = bool;
      default = true;
      description = ''
        Whether to prepopulate the full Hackage index (~1.2 GB, truncated to the
        project's `index-state`) into the `CABAL_DIR` used for plan-to-nix.

        cabal parses this index at startup on every invocation — even with
        `active-repositories: :none` — so for projects that resolve nothing from
        Hackage (every dependency pinned as a local package or
        source-repository-package, e.g. via `replace-hackage-tarball-urls`), the
        full index is a large, needless cost (seconds natively, minutes under
        emulation).  Set this false for such projects to prepopulate an empty
        index instead, which cabal loads instantly.  Leave true (the default)
        for normal projects that resolve dependencies from Hackage.
      '';
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
        # `generated-light` builds hadrian's generated sources (primops,
        # deriveConstants, config) without compiling all of GHC, so realising
        # this source-repository-package during plan-to-nix (needed for the v2
        # UnitId) does not force a full compiler build via IFD.
        paths = [ ghc.configured-src (ghc.generated-light or ghc.generated) ];
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
    # stable-haskell `-target` cross compilers (ghc914-sh for a cross
    # target) ship NO boot libraries: the compiler's target package db is
    # empty and the plan-time dummy `ghc-pkg dump` is empty too (see
    # `emptyGlobalPackageDb` in lib/call-cabal-project-to-nix.nix).  Every
    # project using such a compiler therefore builds the boot packages
    # (rts, base, …) from source as part of its own plan.  This block
    # injects the boot package sources and the project configuration the
    # stable-haskell GHC's own stage3 (cross) project uses.
    (let
      shGhc = (config.compilerSelection pkgs.buildPackages).${config.compiler-nix-name};
      shBootInject =
        if config.injectStableHaskellBootPackages != null
          then config.injectStableHaskellBootPackages
          else shGhc.emptyGlobalPackageDb or false;
    in lib.mkIf shBootInject (let
      # The configured GHC source tree.  The BUILD-platform tree is what
      # the `packages:` entries point at (same tree the compiler itself is
      # built from); the eval-platform copy is only read (below) for the
      # boot dep versions, so plan-time text stays eval-light.
      shSrc = shGhc.configured-src;
      # `shGhc` is already the compiler variant selected for this project's
      # `evalSystem` (via `compilerSelection` → `evalWith.${evalSystem}`), so its
      # plain `configured-src-eval` is the eval-platform tree for this project.
      shSrcEval = shGhc.configured-src-eval;
      # (name, version) pairs of the boot deps the stable-haskell stage2
      # project pins as direct hackage tarball URLs.  The same versions are
      # pinned here as exact version constraints, resolved from the
      # project's normal hackage index (the URL form would require
      # `replace-hackage-tarball-urls` on every user project).  Commented
      # URLs in the project file are skipped.
      shUrlBootPkgs = lib.concatMap (line:
          let m = builtins.match ".*(https://hackage[.]haskell[.]org/package/[^/]+/(.*)-([0-9][0-9.]*)[.]tar[.]gz).*" line;
          in lib.optional (m != null && builtins.match "[[:space:]]*--.*" line == null)
            { name = builtins.elemAt m 1; version = builtins.elemAt m 2; })
        (lib.splitString "\n" (builtins.readFile "${shSrcEval}/cabal.project.stage2.merged"));
      # In-tree boot packages, as subdirs of the configured tree.  Mirrors
      # the packages: stanza of the tree's cabal.project.stage3 (the cross
      # project): everything not resolvable from hackage.  utils/ghc-iserv
      # is included for the external TH interpreter; utils/genprimopcode
      # and utils/deriveConstants satisfy lib:ghc's build-tool-depends
      # (build-stage units, they run on the build machine).
      #
      # These are injected as LOCAL `packages:` entries (absolute store
      # paths), NOT as a source-repository-package: cabal hashes an SRP by
      # listing its package sources sdist-style, which fails on
      # ghc-internal's compiler-generated virtual modules
      # (GHC.Internal.Prim & co have no source files, and upstream
      # deliberately leaves them out of `autogen-modules` — see the
      # commented block in ghc-internal.cabal.in).  Local packages skip
      # that hashing and get `<pkg>-<ver>-inplace` unit ids — exactly the
      # arrangement the native ghc914-sh stage2 build (also all-local)
      # already exercises under the v2 builder.
      shSubdirs = [
        "rts-headers" "rts-fs" "rts"
        "compiler"
        "libraries/base"
        "libraries/ghc-bignum"
        "libraries/ghc-boot"
        "libraries/ghc-boot-th"
        "libraries/ghc-compact"
        "libraries/ghc-experimental"
        "libraries/ghc-heap"
        "libraries/ghc-internal"
        "libraries/ghc-platform"
        "libraries/ghc-prim"
        "libraries/ghci"
        "libraries/integer-gmp"
        "libraries/system-cxx-std-lib"
        "libraries/template-haskell"
        "utils/genprimopcode"
        "utils/deriveConstants"
        "utils/ghc-iserv"
      ];
      tp = pkgs.stdenv.hostPlatform;
      isWasm = tp.isWasm or false;
      # Extra fields appended inside `package` stanzas below.  Built with
      # explicit "\n  " (two-space) indentation — nix's `''` indentation
      # stripping applies to nested `''` strings separately, which would
      # push these lines to column 0 and change their cabal scope from the
      # stanza to the whole project.
      rtsWasmExtras = lib.optionalString isWasm ("\n"
        + "  -- GHC's wasm backend uses JSFFI, but the rts C sources still\n"
        + "  -- include ffi.h; nixpkgs' wasi toolchain ships no libffi, so\n"
        + "  -- point at the libffi-wasm build (same one hadrian wasm\n"
        + "  -- bindists use).\n"
        + "  extra-include-dirs: ${shGhc.libffi-wasm.dev}/include\n"
        + "  extra-lib-dirs: ${shGhc.libffi-wasm.out}/lib");
      crossLinkFields = lib.optionalString (!isWasm) ("\n"
        + "  shared: ${if shGhc.enableShared or false then "True" else "False"}\n"
        + "  executable-dynamic: False");
    in {
      # The boot packages resolve in the HOST stage against an empty
      # installed set, while build-tool-depends / setup-depends resolve in
      # the BUILD stage against the (fully populated) native compiler — a
      # two-stage plan (the fork's `--with-build-compiler`).
      withBuildCompiler = true;
      # rts needs per-file Cmm options (`cmm-options:` stanzas) that only
      # the v2 (cabal v2-build slicing) builder applies — v1's standalone
      # Setup.hs links a mainline Cabal that drops them.  Hard-set (not
      # mkDefault) so an explicit `builderVersion = 1` fails loudly
      # instead of building a subtly broken rts.
      builderVersion = 2;
      cabalProjectLocal = lib.mkBefore ''
        -- Added by the stable-haskell boot-package injection (see the
        -- `injectStableHaskellBootPackages` option): the compiler ships no
        -- target boot libraries, so the plan builds them from source.
        packages:
        ${lib.concatMapStrings (d: "  ${shSrc}/${d}\n        ") shSubdirs}
        -- Forked boot packages that live outside the GHC tree.
        source-repository-package
          type: git
          location: https://github.com/stable-haskell/Cabal.git
          tag: stable-haskell/master
          subdir: Cabal Cabal-syntax

        -- hsc2hs with batch cross-compilation support (a build-stage tool).
        source-repository-package
          type: git
          location: https://github.com/stable-haskell/hsc2hs.git
          tag: d07eea1260894ce5fe456f881fbc62366c9eb1b7

        -- Mirrors cabal.project.stage2.common: the boot packages' bounds
        -- predate base-4.22 etc.
        allow-newer: hsc2hs:*, Win32:*, array:*, binary:*, bytestring:*,
                     containers:*, deepseq:*, directory:*, exceptions:*,
                     file-io:*, filepath:*, haskeline:*, hpc:*,
                     libffi-clib:*, mtl:*, os-string:*, parsec:*, pretty:*,
                     process:*, semaphore-compat:*, stm:*, terminfo:*,
                     text:*, time:*, transformers:*, template-haskell-lift:*,
                     template-haskell-quasiquoter:*, unix:*, xhtml:*

        constraints:
          -- Build-stage dependencies come installed from the native
          -- compiler (mirrors cabal.project.stage3).
          build:any.ghc-internal installed
          -- The solver otherwise believes these are installed for the
          -- target although they only exist in the build compiler's db
          -- (mirrors cabal.project.stage3).
          , Cabal source, Cabal-syntax source, array source, base source
          , binary source, bytestring source, containers source
          , deepseq source, directory source, exceptions source
          , file-io source, filepath source, ghc-bignum source, hpc source
          , integer-gmp source, mtl source, os-string source, parsec source
          , pretty source, process source, rts source, rts-headers source
          , rts-fs source, stm source, system-cxx-std-lib source
          , template-haskell source, text source, time source
          , transformers source, unix source, xhtml source, Win32 source
          -- Exact versions of the hackage-resolved boot deps, matching the
          -- compiler's own stage2 pins.
          ${lib.concatMapStrings (p: ", ${p.name} ==${p.version}\n  ") shUrlBootPkgs}

        package libffi-clib
          ghc-options: -no-rts -optc-Wno-error

        package ghc
          flags: +build-tool-depends +internal-interpreter

        package ghc-bin
          flags: +internal-interpreter

        package ghci
          flags: +internal-interpreter${lib.optionalString isWasm " +use-system-libffi"}

        package ghc-internal
          flags: +bignum-native
          ghc-options: -no-rts

        package rts
          ghc-options: -no-rts
          flags: ${if isWasm then "+use-system-libffi -tables-next-to-code" else "+tables-next-to-code"}${rtsWasmExtras}

        package rts-headers
          ghc-options: -no-rts

        package rts-fs
          ghc-options: -no-rts

        package *
          library-for-ghci: False${crossLinkFields}
      '';
      inputMap = {
        "https://github.com/stable-haskell/Cabal.git/stable-haskell/master" =
          pkgs.haskell-nix.sources.ghc914-sh-cabal;
        "https://github.com/stable-haskell/hsc2hs.git/d07eea1260894ce5fe456f881fbc62366c9eb1b7" =
          pkgs.haskell-nix.sources.ghc914-sh-hsc2hs;
      };
      # rts/configure AC_PATH_PROGs programs that are not cabal-level
      # dependencies (DERIVE_CONSTANTS, GENAPPLY, NM, OBJDUMP, PYTHON).
      # The v2 builder runs configure through cabal (no preConfigure
      # hooks), so they must be on the rts slices' PATH — build-tools feed
      # the slice's extraNativeBuildInputs.  The tools come from the
      # compiler's passthru (built natively; nm/objdump handle TARGET
      # objects).
      modules = [ ({ config, lib, ... }: {
        packages = lib.optionalAttrs (config.packages ? rts) {
          rts.components = {
            library.build-tools = shGhc.bootPkgTools or [];
            sublibs = lib.genAttrs
              [ "nonthreaded-nodebug" "threaded-nodebug"
                "nonthreaded-debug" "threaded-debug" ]
              (_: { build-tools = shGhc.bootPkgTools or []; });
          };
        };
      }) ];
    }))
  ];
}
