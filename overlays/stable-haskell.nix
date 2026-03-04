# overlays/stable-haskell.nix
#
# Builds `haskell-nix.compiler.sghc914` (stable-haskell GHC 9.14) using
# haskell-nix.cabalProject' to replicate the two-stage cabal build described in
# ../stable-ghc-9.14/Makefile (without invoking Hadrian).
#
# Stage flow:
#   configuredSrc  -- autoconf/configure, ghc-boot-th-next synthesis,
#                     FIXME patches, import inlining
#   stage1Project  -- cabalProject' with bootGhc (ghc9103)
#                     builds: GHC executables, ghc-toolchain-bin
#   stage1Compiler -- wrapper scripts + empty package DB
#   stage2Project  -- cabalProject' with ghcOverride = stage1Compiler
#                     builds: all boot libraries + GHC executables
#   sghc914        -- final compiler with populated package DB

final: prev:
let
  pkgs         = final;
  lib          = final.lib;
  evalPackages = pkgs.buildPackages;

  bootGhcName      = "ghc9103";
  ghcVersionFull   = "9.14.0";
  ghcVersionMunged = "9.14";
  ghcVersionForLib = "9.1400";

  # ── Cross-compilation detection ──────────────────────────────────────────
  # isCrossTarget: targetPlatform != hostPlatform
  #   e.g. pkgsCross.wasi32.buildPackages (host=native, target=wasm32)
  #   → we must BUILD a cross-targeting sghc914 here
  # isCrossHost: buildPlatform != hostPlatform
  #   e.g. pkgsCross.wasi32 (build=native, host=wasm32)
  #   → can't build a compiler that runs on wasm32; skip
  isCrossTarget = pkgs.stdenv.targetPlatform != pkgs.stdenv.hostPlatform;
  isCrossHost   = pkgs.stdenv.buildPlatform != pkgs.stdenv.hostPlatform;

  # Target platform for cross-compilation settings
  tp = pkgs.stdenv.targetPlatform;

  # Platform values needed to patch the FIXME placeholders in
  # cabal.project.common.  These describe the BUILD machine (i.e. the machine
  # running the compiler), which for a native build is the host platform.
  hp           = pkgs.stdenv.hostPlatform;
  buildTriple  = hp.config;   # e.g. "aarch64-apple-darwin"
  buildArch    =
    if hp.isAarch64 then "aarch64"
    else if hp.isx86_64 then "x86_64"
    else "unknown";
  buildOS      =
    if hp.isDarwin  then "darwin"
    else if hp.isLinux   then "linux"
    else if hp.isWindows then "mingw32"
    else "unknown";
  buildVendor =
    if hp.isDarwin then "apple"
    else if hp.isLinux then "unknown"
    else "unknown";
  ghcUnregisterised = "NO";
  tablesNextToCode  = "YES";

  sghc914Src = pkgs.haskell-nix.sources.sghc914;

  # ── External tools (built with boot GHC) ──────────────────────────────────
  # alex and happy are needed by compiler/Setup.hs (via +build-tool-depends)
  # to generate the parser/lexer files for the GHC compiler package.
  alexTool  = pkgs.haskell-nix.tool bootGhcName "alex"  { version = "3.5.2.0"; inherit evalPackages; };
  happyTool = pkgs.haskell-nix.tool bootGhcName "happy" { version = "2.1.5";   inherit evalPackages; };

  # genprimopcode is built from the GHC 9.14 in-tree source because the boot
  # GHC 9.10.3's copy doesn't understand attributes new in 9.14 (e.g.
  # defined_bits).  It only depends on base + array, with alex/happy for
  # Lexer.x / Parser.y (resolved from Hackage by haskell-nix.tool).
  genPrimopCodeTool = pkgs.haskell-nix.tool bootGhcName "genprimopcode" {
    src = "${configuredSrc}/utils/genprimopcode";
    inherit evalPackages;
  };

  # deriveConstants is built from the GHC 9.14 in-tree source for the same
  # reason as genprimopcode: the boot GHC 9.10.3's copy generates
  # GHC.Platform.Constants.hs with fewer constants than GHC 9.14 expects.
  # Stage1's compiler/Setup.hs calls deriveConstants to generate
  # GHC.Platform.Constants.hs; if boot GHC's deriveConstants runs instead
  # (because the boot bin dir is first in PATH), the resulting stage1 GHC
  # binary's parseConstantsHeader expects N_9.10 constants.  Stage2
  # rts.preConfigure then calls s1exe "deriveConstants" (GHC 9.14 version)
  # to generate DerivedConstants.h with N_9.14 > N_9.10 constants → panic
  # "Invalid platform constants" during the rts-lib-rts install phase.
  # Prepending this tool to PATH in preConfigure ensures the in-tree (9.14)
  # deriveConstants is used, so both .hs and .h files are generated with the
  # same N_9.14 constant count.
  deriveConstantsTool = pkgs.haskell-nix.tool bootGhcName "deriveConstants" {
    src = "${configuredSrc}/utils/deriveConstants";
    inherit evalPackages;
  };

  # ── Configured source ──────────────────────────────────────────────────────
  # Runs autoconf/configure to generate .cabal files from .cabal.in,
  # synthesises libraries/ghc-boot-th-next, patches the FIXME platform
  # placeholders in cabal.project.common, and produces self-contained
  # (import-inlined) project files.
  configuredSrc = pkgs.stdenv.mkDerivation {
    name = "sghc914-configured-src";
    src  = sghc914Src;

    nativeBuildInputs = with pkgs; [ autoconf automake libtool python3 ];

    # Nix's default unpackPhase is fine; skip configure/install phases.
    phases = [ "unpackPhase" "buildPhase" "installPhase" ];

    buildPhase = ''
      # ── 1. Synthesise libraries/ghc-boot-th-next ─────────────────────────
      # The stage1 build uses ghc-boot-th-next (= ghc-boot-th with +bootstrap
      # flag) to avoid the ghc-internal dependency.  ghc-boot-th-next is listed
      # in AC_CONFIG_FILES so configure will process its .cabal.in → .cabal,
      # substituting all @VAR@ placeholders.  We copy the full source directory
      # (not just changelog/LICENSE) so Haskell modules are available.
      cp -r libraries/ghc-boot-th libraries/ghc-boot-th-next
      rm -f libraries/ghc-boot-th-next/ghc-boot-th.cabal.in
      sed 's/ghc-boot-th@Suffix@/ghc-boot-th-next/g' \
          libraries/ghc-boot-th/ghc-boot-th.cabal.in \
          > libraries/ghc-boot-th-next/ghc-boot-th-next.cabal.in

      # ── 2. Run autoconf + configure ─────────────────────────────────────
      # autoreconf generates ./configure from configure.ac and the m4/ macros.
      autoreconf -i

      # Several submodule libraries use build-type: Configure with their own
      # configure.ac files.  Run autoreconf -i in each so the configure script
      # is present before haskell.nix builds them.  (libffi-clib needs libtool;
      # all others only need autoconf/automake, but having libtool available
      # is harmless — autoreconf only calls libtoolize when LT_INIT is present.)
      for libdir in \
          libraries/libffi-clib \
          libraries/unix \
          libraries/directory \
          libraries/process \
          libraries/time \
          libraries/terminfo \
          libraries/ghc-internal \
          rts; do
        if [ -f "$libdir/configure.ac" ]; then
          (cd "$libdir" && autoreconf -i 2>&1 | tail -1 || true)
        fi
      done


      # configure generates config.status AND all AC_CONFIG_FILES (the .cabal
      # files, ghcversion.h, cabal.project.stage2.settings, …).
      ./configure \
        --with-project-version="${ghcVersionFull}" \
        --with-project-version-int="914" \
        --with-project-version-munged="${ghcVersionMunged}" \
        --with-project-version-for-lib="${ghcVersionForLib}" \
        --with-project-patch-level="0" \
        --with-project-patch-level1="0" \
        --with-project-patch-level2="0"

      # ── 3. Patch FIXME platform values in cabal.project.common ───────────
      # The file contains hard-coded "FIXME" strings for BuildPlatform etc.
      # that must be replaced before the project files are merged.
      sed -i \
        -e 's|DBuildPlatform=\\"FIXME\\"|DBuildPlatform=\\"${buildTriple}\\"|' \
        -e 's|DBuildArch=\\"FIXME\\"|DBuildArch=\\"${buildArch}\\"|' \
        -e 's|DBuildOS=\\"FIXME\\"|DBuildOS=\\"${buildOS}\\"|' \
        -e 's|DBuildVendor=\\"FIXME\\"|DBuildVendor=\\"${buildVendor}\\"|' \
        -e 's|GhcUnregisterised=\\"FIXME\\"|GhcUnregisterised=\\"${ghcUnregisterised}\\"|' \
        -e 's|TablesNextToCode=\\"FIXME\\"|TablesNextToCode=\\"${tablesNextToCode}\\"|' \
        cabal.project.common

      # ── 4. Inline imports → self-contained project files ─────────────────
      # haskell.nix reads cabalProject as a string and writes it to a temp
      # location, so "import:" directives would not resolve.  We inline them
      # here so the merged files are self-contained.
      inline_imports() {
        local f="$1"
        local line imp
        while IFS= read -r line; do
          if imp=$(printf '%s' "$line" \
                   | sed -n 's/^[[:space:]]*import:[[:space:]]*\(.*[^[:space:]]\)[[:space:]]*$/\1/p') \
             && [ -n "$imp" ]; then
            if [ -f "$imp" ]; then
              cat "$imp"
            else
              printf '%s\n' "$line"
            fi
          else
            printf '%s\n' "$line"
          fi
        done < "$f"
      }

      inline_imports cabal.project.stage1 > cabal.project.stage1.merged

      # cabal.project.stage1 constrains 'template-haskell <= 2.22'.  Cabal's
      # solver treats version triples as lists (so 2.22.0.0 > 2.22 because
      # the longer list is greater), causing any GHC 9.10+ boot compiler to
      # fail with its installed template-haskell-2.22.0.0 (or 2.23.0.0 on
      # 9.12+).  Widen to '< 3' to accept whatever the boot GHC provides.
      sed -i 's/template-haskell <= 2\.22/template-haskell < 3/' cabal.project.stage1.merged

      # For stage2: inline imports and strip the direct-URL alex/happy/happy-lib
      # package entries — haskell.nix will resolve them from the Hackage index
      # instead (see cabalProjectLocal in stage2Project below).
      inline_imports cabal.project.stage2 \
        | grep -v 'hackage.haskell.org/package/alex\|hackage.haskell.org/package/happy' \
        > cabal.project.stage2.merged

      # The constraint 'build:any.ghc-internal installed' uses a 'build:' role
      # qualifier that cabal-install 3.16 does not recognise (parser expects
      # 'setup.' as the only valid qualifier).  Since our stage1 package DB is
      # intentionally empty (no installed ghc-internal) the constraint would be
      # impossible to satisfy anyway.  Strip it so cabal can parse the file.
      sed -i '/build:any\.ghc-internal/d' cabal.project.stage2.merged
    '';

    installPhase = "cp -r . $out";
  };

  # ── Stage 1 ───────────────────────────────────────────────────────────────
  # Builds GHC executables (ghc, ghc-pkg, ghc-toolchain-bin, …) using the
  # boot compiler.  The +bootstrap flag on ghc/ghci/ghc-boot/ghc-boot-th-next
  # avoids any dependency on ghc-internal.

  stage1Project = pkgs.haskell-nix.cabalProject' {
    name                 = "sghc914-stage1";
    src                  = configuredSrc;
    cabalProjectFileName = "cabal.project.stage1.merged";
    compiler-nix-name    = bootGhcName;
    inherit evalPackages;
    # Stage1 only needs executables.  Disable tests/benchmarks so cabal
    # doesn't try to resolve test-only deps that conflict (e.g. temporary
    # conflicts with directory-1.3.10.0 via file-io's test suite).
    configureArgs = "--disable-tests --disable-benchmarks";
    modules = [{
      # libffi-clib uses build-type: Configure with AX_ENABLE_BUILDDIR in its
      # configure.ac.  When configure is run from the source directory
      # (srcdir = "."), AX_ENABLE_BUILDDIR re-execs in a host-triple
      # subdirectory (e.g. aarch64-apple-darwin25.3.0/) so all generated
      # files — including libffi-clib.buildinfo — land there, NOT in the
      # package root.
      #
      # Cabal's autoconfUserHooks.confHook calls getHookedBuildInfo which reads
      # libffi-clib.buildinfo from the package root.  Fix: run configure in
      # preConfigure, then promote all three generated artifacts to the package
      # root BEFORE autoconfUserHooks.confHook runs.  autoconfUserHooks will
      # re-run configure (which regenerates the files in the subdirectory,
      # NOT overwriting our root copies) and then find the pre-promoted
      # libffi-clib.buildinfo in the root.
      #
      # The .buildinfo contains asm-sources: src/aarch64/sysv.S (on AArch64),
      # which defines _ffi_closure_trampoline_table_page.  Without this symbol,
      # GHC 9.14 binaries crash on macOS with "symbol not found in flat namespace".
      packages.libffi-clib.preConfigure = ''
        ./configure 2>&1 | tail -3 || true
        # AX_ENABLE_BUILDDIR puts all generated files into a host-triple
        # subdirectory.  Promote them to the package root.
        fficonfig=$(find . -maxdepth 2 -name fficonfig.h 2>/dev/null | head -1)
        [ -n "$fficonfig" ] && cp "$fficonfig" include/fficonfig.h
        build_dir=$(dirname "$fficonfig" 2>/dev/null)
        [ -f "$build_dir/include/ffi.h" ] && cp "$build_dir/include/ffi.h" include/ffi.h
        [ -f "$build_dir/libffi-clib.buildinfo" ] && cp "$build_dir/libffi-clib.buildinfo" libffi-clib.buildinfo
      '';

      # ghc-boot/Setup.hs embeds a git commit hash.  It reads $GIT_COMMIT_ID
      # from the environment first; only if that is unset does it call `git`.
      # There is no git repo in the Nix sandbox, so provide a dummy value.
      packages.ghc-boot.preConfigure = ''
        export GIT_COMMIT_ID="0000000000000000000000000000000000000000"
      '';

      # ── packages.ghc setup isolation ─────────────────────────────────────
      # The stage1 cabal.project includes `packages: libraries/Cabal/Cabal`,
      # so Cabal 3.14+ is built from source and is NOT in preExistingPkgs.
      # Without an override, the ghc setup's package DB gets Cabal 3.14+
      # (from the plan) instead of the boot GHC's Cabal 3.12.  The boot
      # GHC 9.10.3's Setup.hs (set by configuration-nix.nix) uses Cabal
      # 3.12 API (FilePath-based), which is incompatible with Cabal 3.14+
      # (SymbolicPath-based).
      #
      # Fix: override nonReinstallablePkgs so Cabal (and friends) come from
      # the boot GHC's package DB, and clear setup-depends so the plan's
      # Cabal 3.14+ doesn't leak into the setup's package DB.
      nonReinstallablePkgs = [
        "rts" "ghc-prim" "integer-gmp" "integer-simple" "base"
        "ghc-bignum" "system-cxx-std-lib" "ghc-internal"
        "ghc-boot" "ghc" "Cabal" "array" "binary" "bytestring"
        "containers" "deepseq" "directory" "filepath" "ghc-boot-th"
        "ghc-compact" "ghc-heap" "hpc" "mtl" "parsec" "pretty"
        "process" "stm" "exceptions" "semaphore-compat" "os-string"
        "template-haskell" "text" "time" "transformers" "unix"
        "xhtml" "terminfo"
      ];
      packages.ghc.components.setup.depends = lib.mkForce [];
      packages.ghc.package.setup-depends = lib.mkForce [];

      # ghc-boot also has build-type: Custom — its Setup.hs imports
      # System.Directory, System.FilePath, Distribution.System.  Without
      # clearing setup.depends, both the boot GHC and plan versions of
      # directory, filepath, Cabal-syntax end up in the setup's package
      # DB, causing "Ambiguous module name" errors.
      packages.ghc-boot.components.setup.depends = lib.mkForce [];
      packages.ghc-boot.package.setup-depends = lib.mkForce [];

      # ── packages.ghc source override ───────────────────────────────────────
      # configuration-nix.nix sets packages.ghc.src to the boot GHC's
      # configured-src + "/compiler" (lib.mkForce = priority 50).  Stage1
      # must build GHC 9.14's compiler library, not the boot GHC's.
      # mkOverride 49 beats mkForce.
      packages.ghc.src = lib.mkOverride 49 (configuredSrc + "/compiler");

      # setup-builder.nix compiles Setup.hs directly with `ghc --make`,
      # NOT via Cabal, so MIN_VERSION_* macros are not auto-generated.
      # GHC 9.14's Setup.hs uses #if MIN_VERSION_Cabal(3,14,0) etc.
      # Insert the macro definition AFTER language pragmas but BEFORE the
      # first #if directive.  The macro evaluates to TRUE for Cabal 3.12
      # (boot GHC) which is what the setup actually compiles against.
      packages.ghc.components.setup.preBuild = ''
        if [ -f Setup.hs ]; then
          awk '/^#if/ && !ins {print "#define MIN_VERSION_Cabal(a,b,c) ((a)<3||((a)==3&&((b)<12||((b)==12&&(c)<=1))))"; ins=1} {print}' Setup.hs > Setup.hs.tmp
          mv Setup.hs.tmp Setup.hs
        fi
      '';

      # The ghc library's Setup.hs invokes genprimopcode and deriveConstants.
      # The boot GHC 9.10.3's copies predate GHC 9.14 (genprimopcode doesn't
      # understand 'defined_bits'; deriveConstants generates fewer platform
      # constants).  The boot GHC's bin dir comes first in PATH so
      # build-tools resolution won't help; explicitly prepend the in-tree
      # versions in preConfigure so they shadow any boot-installed copies.
      packages.ghc.preConfigure = ''
        export PATH=${genPrimopCodeTool}/bin:${deriveConstantsTool}/bin:$PATH
      '';

      # Two patches to GHCi/Message.hs and GHCi/Run.hs needed when
      # bootstrapping with GHC 9.10.x (base-4.20):
      #
      # (1) GHCi/Message.hs — Binary ClosureType orphan instance:
      #   When HAVE_GHC_INTERNAL is set (impl(ghc > 9.10)), InfoProv has 8
      #   fields including GHC.Internal.ClosureTypes.ClosureType.  Neither
      #   boot ghc-internal-9.1003.0 nor the in-tree source provides a Binary
      #   instance for ClosureType; add it as an orphan (Generic default).
      #
      # (2) GHCi/Run.hs — catchNoPropagate / rethrowIO stubs:
      #   These functions were added in base-4.21 (GHC 9.12).  When building
      #   with boot GHC 9.10.x (base-4.20) they are absent.  Provide trivial
      #   implementations: catchNoPropagate = catch  and  rethrowIO = throwIO.
      #   base-4.20 already allows catching ExceptionWithContext via `catch`,
      #   and ExceptionWithContext is a valid Exception, so throwIO works too.
      packages.ghci.prePatch = ''
        # ── (1) GHCi/Message.hs ─────────────────────────────────────────────
        awk '/^import GHC.InfoProv$/{
          print
          print "#ifdef HAVE_GHC_INTERNAL"
          print "import GHC.Internal.ClosureTypes (ClosureType)"
          print "#endif"
          next
        }1' GHCi/Message.hs > GHCi/Message.hs.tmp \
          && mv GHCi/Message.hs.tmp GHCi/Message.hs

        awk '/^instance Binary InfoProv where$/{
          print "#ifdef HAVE_GHC_INTERNAL"
          print "instance Binary ClosureType"
          print "#endif"
        }1' GHCi/Message.hs > GHCi/Message.hs.tmp \
          && mv GHCi/Message.hs.tmp GHCi/Message.hs

        # ── (2) GHCi/Run.hs ─────────────────────────────────────────────────
        awk '/^rethrow :: EvalOpts -> IO a -> IO a$/{
          print "#if !MIN_VERSION_base(4,21,0)"
          print "catchNoPropagate :: IO a -> (ExceptionWithContext SomeException -> IO a) -> IO a"
          print "catchNoPropagate = catch"
          print "rethrowIO :: ExceptionWithContext SomeException -> IO a"
          print "rethrowIO = throwIO"
          print "#endif"
        }1' GHCi/Run.hs > GHCi/Run.hs.tmp \
          && mv GHCi/Run.hs.tmp GHCi/Run.hs
      '';
    }];
  };

  s1 = stage1Project.hsPkgs;

  # Convenience: path to a stage1 executable.
  s1exe = pkg: exe: s1.${pkg}.components.exes.${exe}.exePath;

  # Assemble the stage1 compiler: wrapper scripts that override the package DB
  # path (cabal puts the wrong GhcLibDir into the compiled-in settings), plus
  # an empty package DB (stage2 lists all packages as local so cabal builds
  # them all from source).
  # makeCompilerDeps wraps the runCommand derivation with a cachedDeps
  # attribute (a build-time derivation that runs `ghc-pkg list` and records
  # package IDs/versions).  Wrapping here (rather than relying on the
  # cache-compiler-deps overlay) ensures cachedDeps is present on the local
  # let binding used by stage2Project's ghcOverride — without it every
  # stage2 library build emits "WARNING: ghc.cachedDeps not found".
  stage1Compiler =
    let rawDrv = pkgs.runCommand "sghc914-stage1-compiler" {
      passthru = {
        version              = ghcVersionFull;
        targetPrefix         = "";
        haskellCompilerName  = "ghc-${ghcVersionFull}";
        isHaskellNixCompiler = true;
        libDir               = "lib/ghc-${ghcVersionFull}";
        project              = stage1Project;
        # comp-builder.nix:44 accesses ghc.enableShared to decide whether to
        # build shared Haskell libraries.  Stage1/2 use shared libs on Darwin.
        enableShared         = !pkgs.stdenv.hostPlatform.isMusl && !pkgs.stdenv.hostPlatform.isStatic;
        # call-cabal-project-to-nix.nix:297 needs raw-src to locate bundled
        # package .cabal files.  Our configuredSrc is the fully-patched GHC
        # source tree with all .cabal files generated, which is exactly what
        # is needed.  raw-src is a function of evalPackages (ignored here).
        raw-src              = _: configuredSrc;
        # builder/default.nix:29 uses `ghc.buildGHC or ghc` for setup-builder.
        # Setup.hs compilation (for buildType: Configure/Custom packages) requires
        # `base` and `Cabal` to be registered in the GHC's package DB.  Our
        # stage1Compiler has an empty package DB, so we direct setup-builder to
        # use the boot GHC (which has these packages) instead.
        #
        # The ghc package's custom Setup.hs imports containers (Data.Map).
        # The stage1/stage2 modules clear packages.ghc.{components.setup,
        # package.setup}-depends so the setup's package DB only contains
        # packages from the boot GHC's nonReinstallablePkgs — avoiding the
        # ghc-internal-9.1400.0 "Invalid platform constants" panic.
        buildGHC             = pkgs.haskell-nix.compiler.${bootGhcName};
        # haskell.nix calls ghc.override { ghcEvalPackages = evalPackages; }
        # when isHaskellNixCompiler is true (overlays/haskell.nix:699).
        # override references the outer stage1Compiler (with cachedDeps) so
        # callers always get the fully-formed version.
        override             = _: stage1Compiler;
      };
      # Make bintools (ranlib, nm, ar, otool, install_name_tool) available in
      # PATH so ghc-toolchain-bin can auto-discover them.  Also make the C
      # compiler wrapper available so cc/c++ are in PATH.
      nativeBuildInputs = [ pkgs.stdenv.cc pkgs.stdenv.cc.bintools.bintools ];
    } ''
    mkdir -p $out/bin
    mkdir -p $out/lib/ghc-${ghcVersionFull}
    # Note: do NOT mkdir package.conf.d here; ghc-pkg init creates it below.

    # ── ghc wrapper ──────────────────────────────────────────────────────────
    # Pass -no-global-package-db so GHC ignores the compiled-in (wrong) libdir
    # and uses our empty package DB instead.
    #
    # -no-rts bypasses GHC 9.14's mkUnitState check for rts sub-libraries
    # (rts:nonthreaded-nodebug etc.) in the package DB.  During the stage2
    # rts build, the rts sub-libraries are not yet registered when GHC is
    # invoked by Cabal's installLib (e.g. for shared/GHCi library steps).
    # Without -no-rts, GHC 9.14 panics: "The RTS for rts:nonthreaded-nodebug
    # is missing from the package database while building unit rts".
    # This flag only suppresses the validation check; it does not affect
    # actual linking (the rts is still linked normally once registered).
    cat > $out/bin/ghc << 'ENDSCRIPT'
#!/bin/sh
exec GHC_EXE \
  -no-global-package-db \
  -package-db PACKAGEDB \
  -BLIBDIR \
  -no-rts \
  "$@"
ENDSCRIPT
    sed -i \
      -e "s|GHC_EXE|${s1exe "ghc-bin" "ghc"}|" \
      -e "s|PACKAGEDB|$out/lib/ghc-${ghcVersionFull}/package.conf.d|" \
      -e "s|LIBDIR|$out/lib/ghc-${ghcVersionFull}|" \
      $out/bin/ghc
    chmod +x $out/bin/ghc

    # ghci is just ghc in interactive mode
    ln -s $out/bin/ghc $out/bin/ghci

    # ── ghc-pkg wrapper ───────────────────────────────────────────────────────
    # Wrap ghc-pkg to always use our own (empty) package DB.  Without this
    # makeCompilerDeps (haskell.nix's cachedDeps mechanism) would call
    # ghc-pkg list using the compiled-in libdir of the exe derivation, which
    # has no package DB and would fail or list the wrong packages.
    cat > $out/bin/ghc-pkg << 'ENDSCRIPT'
#!/bin/sh
exec GHC_PKG_EXE \
  --no-user-package-db \
  --global-package-db PACKAGEDB \
  "$@"
ENDSCRIPT
    sed -i \
      -e "s|GHC_PKG_EXE|${s1exe "ghc-pkg" "ghc-pkg"}|" \
      -e "s|PACKAGEDB|$out/lib/ghc-${ghcVersionFull}/package.conf.d|" \
      $out/bin/ghc-pkg
    chmod +x $out/bin/ghc-pkg

    # ── Initialise empty package DB ───────────────────────────────────────────
    # Use the raw binary so the wrapper does not pass --global-package-db for
    # a path that does not yet exist.
    ${s1exe "ghc-pkg" "ghc-pkg"} init $out/lib/ghc-${ghcVersionFull}/package.conf.d

    # ── Additional tools needed by stage2 packages ───────────────────────────
    # hsc2hs is needed by rts-fs and other packages; unlit is a standard GHC
    # distribution tool.  ghc-for-component-wrapper lndir's the whole GHC
    # bin dir, so any tool placed here becomes available in stage2 builds.
    # (runghc is not a stage1 tool — it requires boot libraries.)
    ln -sf ${s1exe "hsc2hs" "hsc2hs"}  $out/bin/hsc2hs
    ln -sf ${s1exe "unlit"  "unlit"}   $out/bin/unlit
    # GHC 9.14 computes the unlit path as $topdir/../bin/unlit where topdir is
    # lib/ghc-9.14.0 (set by -B$NIX_GHC_LIBDIR in ghc-for-component-wrapper).
    # lib/ghc-9.14.0/../bin/unlit resolves to lib/bin/unlit, NOT bin/unlit.
    # ghc-for-component-wrapper.nix's lndir copies lib/bin/ from the unwrapped
    # GHC, so we must provide lib/bin/unlit (and hsc2hs) here.
    mkdir -p $out/lib/bin
    ln -sf ${s1exe "unlit"  "unlit"}   $out/lib/bin/unlit
    ln -sf ${s1exe "hsc2hs" "hsc2hs"} $out/lib/bin/hsc2hs

    # ── settings file (generated by ghc-toolchain-bin) ────────────────────────
    # ghc-toolchain-bin queries the C compiler and generates a settings file
    # describing the toolchain.  Stage2 needs this so GHC can invoke CC etc.
    #
    # ghc-toolchain-bin calls `sh config.sub <triple>` to normalise the target
    # triple; config.sub must be in the current directory (sh looks there for
    # non-absolute script arguments).  Copy it from the configured source tree.
    cp ${configuredSrc}/config.sub ./config.sub
    ${s1exe "ghc-toolchain-bin" "ghc-toolchain-bin"} \
      --disable-ld-override \
      --triple ${buildTriple} \
      --cc  $(type -P cc) \
      --cxx $(type -P c++) \
      --output-settings \
      -o $out/lib/ghc-${ghcVersionFull}/settings

    # ── ghcversion.h ─────────────────────────────────────────────────────────
    # The stage2 build needs the generated ghcversion.h header.
    mkdir -p $out/lib/ghc-${ghcVersionFull}/include
    cp ${configuredSrc}/rts/include/ghcversion.h \
       $out/lib/ghc-${ghcVersionFull}/include/
    '';
    in pkgs.haskell-nix.haskellLib.makeCompilerDeps rawDrv;

  # ── Stage 2 ───────────────────────────────────────────────────────────────
  # Builds all GHC boot libraries and executables using the stage1 compiler.

  stage2Project = pkgs.haskell-nix.cabalProject' {
    name                 = "sghc914-stage2";
    src                  = configuredSrc;
    cabalProjectFileName = "cabal.project.stage2.merged";
    # Use the stage1 compiler (exported as sghc914-stage1) to build
    # all stage2 boot libraries and executables.
    compiler-nix-name    = "sghc914-stage1";
    inherit evalPackages;
    # The plan step passes --enable-tests --enable-benchmarks by default.
    # GHC 9.14's base-4.22 is not yet in the transitive test-dep closure of
    # packages like `unix` (splitmix upper-bounds), so resolution would fail.
    # Disable tests and benchmarks for the plan step; they are not needed for
    # building the boot libraries.
    configureArgs = "--disable-tests --disable-benchmarks";

    # Re-enable Hackage so alex, happy, and happy-lib can be found for
    # build-tool-depends resolution.  They were direct-URL packages in the
    # original cabal.project.stage2 but were stripped above.
    # active-repositories in this local override wins over the ":none" in
    # the merged project file (later stanzas take precedence in cabal).
    cabalProjectLocal = ''
      active-repositories: hackage.haskell.org

      -- GHC 9.14's mkUnitState wires the meta-rts to rts:nonthreaded-nodebug
      -- when loading any package DB that contains the rts conf.  The rts
      -- sub-library packages (threaded-nodebug, nonthreaded-debug,
      -- threaded-debug) have build-depends: rts, so they trigger the wiring;
      -- but rts:nonthreaded-nodebug is not yet in their build package DB
      -- (it's what we're building).  -no-rts bypasses the wiring check
      -- (explicitly suggested by the error message itself).  The main rts
      -- library is also compiled with -no-rts so Cabal's install phase does
      -- not invoke GHC in a way that triggers the same check.
      package rts
        ghc-options: -no-rts
    '';

    modules = [{
      # The ghc package's custom Setup.hs imports containers, directory,
      # filepath, Cabal, process — all boot packages.  These must be in
      # nonReinstallablePkgs so makeSetupConfigFiles copies them from the
      # boot GHC's package DB (not from the plan, which would resolve to
      # stage2-built versions that depend on ghc-internal-9.1400.0).
      #
      # The nonReinstallablePkgs option uses lib.last merge (last def wins).
      # component-driver.nix sets it from preExistingPkgs (which is non-empty
      # from the plan), ignoring reinstallableLibGhc.  We must override it
      # directly with the full list of boot packages.
      #
      # For library builds this is safe: make-config-files copies these from
      # the GHC's package DB, but the stage1Compiler has an empty DB, so
      # nothing is found.  The plan's deps are then used via library.depends.
      nonReinstallablePkgs = [
        "rts" "ghc-prim" "integer-gmp" "integer-simple" "base"
        "ghc-bignum" "system-cxx-std-lib" "ghc-internal"
        "ghc-boot" "ghc" "Cabal" "array" "binary" "bytestring"
        "containers" "deepseq" "directory" "filepath" "ghc-boot-th"
        "ghc-compact" "ghc-heap" "hpc" "mtl" "parsec" "pretty"
        "process" "stm" "exceptions" "semaphore-compat" "os-string"
        "template-haskell" "text" "time" "transformers" "unix"
        "xhtml" "terminfo"
      ];

      # libffi-clib: same AX_ENABLE_BUILDDIR fix as stage1.  Run configure in
      # preConfigure and promote the generated files (fficonfig.h, ffi.h, and
      # libffi-clib.buildinfo) to the package root so autoconfUserHooks finds
      # libffi-clib.buildinfo there and picks up the platform-specific asm-sources.
      packages.libffi-clib.preConfigure = ''
        ./configure 2>&1 | tail -3 || true
        fficonfig=$(find . -maxdepth 2 -name fficonfig.h 2>/dev/null | head -1)
        [ -n "$fficonfig" ] && cp "$fficonfig" include/fficonfig.h
        build_dir=$(dirname "$fficonfig" 2>/dev/null)
        [ -f "$build_dir/include/ffi.h" ] && cp "$build_dir/include/ffi.h" include/ffi.h
        [ -f "$build_dir/libffi-clib.buildinfo" ] && cp "$build_dir/libffi-clib.buildinfo" libffi-clib.buildinfo
      '';

      # Several GHC bundled libraries (ghc-internal, unix, directory, process,
      # time, terminfo) use build-type: Configure with configure.ac entries
      # containing AC_CONFIG_AUX_DIR([..]) — they expect config.guess,
      # config.sub, install-sh etc. in the PARENT directory.  In the Nix
      # per-package sandbox the parent is an empty temp dir.  Copy the
      # auxiliary files from configuredSrc's top-level (where the top-level
      # autoreconf placed them) to ../ so configure can find them.
      packages.ghc-internal.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${configuredSrc}/$f ] && cp ${configuredSrc}/$f ../ || true
        done
        # Ensure include/ is populated so configure can find md5.h.
        mkdir -p include
        cp -f ${configuredSrc}/libraries/ghc-internal/include/*.h include/ 2>/dev/null || true
        # The configure.ac check AC_CHECK_SIZEOF([struct MD5Context],[],
        # [#include "include/md5.h"]) fails in the Nix sandbox even when
        # include/md5.h is present.  Export the source root's include path
        # via CPPFLAGS so that Cabal's autoconfUserHooks inherits it and
        # the conftest can find "include/md5.h" from the source root.
        export CPPFLAGS="-I$(pwd) ''${CPPFLAGS:-}"
        # Also seed config.cache with the known size in case configure runs
        # from a different working directory (where it creates a new cache).
        echo 'ac_cv_sizeof_struct_MD5Context=88' > config.cache
      '';
      packages.unix.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${configuredSrc}/$f ] && cp ${configuredSrc}/$f ../ || true
        done
      '';
      packages.directory.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${configuredSrc}/$f ] && cp ${configuredSrc}/$f ../ || true
        done
      '';
      packages.process.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${configuredSrc}/$f ] && cp ${configuredSrc}/$f ../ || true
        done
      '';
      packages.time.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${configuredSrc}/$f ] && cp ${configuredSrc}/$f ../ || true
        done
      '';
      packages.terminfo.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${configuredSrc}/$f ] && cp ${configuredSrc}/$f ../ || true
        done
      '';

      # The per-package source root is extracted from the raw sghc914 source
      # (git checkout), which does not include the autoreconf-generated
      # configure script.  Copy it from configuredSrc (where rts autoreconf
      # was run) so autoconfUserHooks can run it to generate ghcautoconf.h,
      # DerivedConstants.h, AutoApply*.cmm.h, EventLog*.h, and rts.buildinfo.
      #
      # rts/configure.ac uses AC_PATH_PROG for five external programs
      # (mirrored from the stable-ghc-9.14/Makefile STAGE2_CABAL_BUILD):
      #   DERIVE_CONSTANTS  – stage1 deriveConstants tool
      #   GENAPPLY          – stage1 genapply tool
      #   NM                – nm (from bintools)
      #   OBJDUMP           – objdump (from bintools)
      #   PYTHON            – python3 (for EventLog header generation)
      # Setting them here ensures they are in the cabal-build environment that
      # Cabal's autoconfUserHooks.confHook inherits when it calls ./configure.
      # The per-package source root is an isolated rts/ directory extracted
      # from configuredSrc.  The rts configure.ac uses AC_CONFIG_AUX_DIR([..])
      # so the generated configure script searches for autoconf auxiliary files
      # (install-sh, config.sub, config.guess) in the PARENT directory.  In the
      # per-package source root layout there is no parent containing these files.
      #
      # Copy the auxiliary files from the configuredSrc top level (where the
      # top-level autoreconf placed them) to "../" (= one level above the rts/
      # source root in the Nix build sandbox) so configure can find them.
      #
      # Running configure explicitly in preConfigure generates ghcautoconf.h,
      # DerivedConstants.h, AutoApply*.cmm.h, and rts/EventLog*.h into
      # $PWD/include/ (= the mutable source copy).  Cabal's install phase
      # looks for install-includes files in the source root's include-dirs
      # ("include" and "."), so the generated headers must be there.
      #
      # autoconfUserHooks also calls configure (with --srcdir=...) — this is
      # harmless since configure is idempotent; it also succeeds because
      # $srcdir/.. = configuredSrc has install-sh.
      packages.rts.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${configuredSrc}/$f ] && cp ${configuredSrc}/$f ../ || true
        done
        cp ${configuredSrc}/rts/configure .
        chmod +x configure
        export DERIVE_CONSTANTS=${s1exe "deriveConstants" "deriveConstants"}
        export GENAPPLY=${s1exe "genapply" "genapply"}
        export NM=${pkgs.stdenv.cc.bintools.bintools}/bin/nm
        export OBJDUMP=${pkgs.stdenv.cc.bintools.bintools}/bin/objdump
        export PYTHON=${pkgs.python3}/bin/python3
        # rts/configure.ac uses AC_DEFINE_UNQUOTED([USE_LIBDW], [$CABAL_FLAG_libdw])
        # and similar for other flags.  If the CABAL_FLAG_xxx variable is unset,
        # AC_DEFINE_UNQUOTED emits '#define USE_LIBDW' with no value, and clang
        # then rejects '#if USE_LIBDW' with "expected value in expression".
        # Set numeric (0/1) values here; Cabal's autoconfUserHooks inherits them
        # when it calls configure again in the configure phase.
        export CABAL_FLAG_libdw=0
        export CABAL_FLAG_libbfd=0
        export CABAL_FLAG_libzstd=0
        export CABAL_FLAG_static_libzstd=0
        export CABAL_FLAG_libnuma=0
        export CABAL_FLAG_unregisterised=0
        export CABAL_FLAG_tables_next_to_code=${if tablesNextToCode == "YES" then "1" else "0"}
        export CABAL_FLAG_leading_underscore=${if hp.isDarwin then "1" else "0"}
        ./configure
      '';

      # ghc-boot has build-type: Custom.  Its Setup.hs:
      # 1. Embeds a git commit hash via confHook — reads $GIT_COMMIT_ID
      # 2. Generates GHC/Platform/Host.hs via postConf
      # Both work naturally with the Custom Setup.hs; we just need to
      # provide GIT_COMMIT_ID (no git in the Nix sandbox).
      packages.ghc-boot.preConfigure = ''
        export GIT_COMMIT_ID="0000000000000000000000000000000000000000"
      '';

      # ── packages.ghc source override ───────────────────────────────────────
      # configuration-nix.nix sets packages.ghc.src to the boot GHC's
      # configured-src + "/compiler" (using lib.mkForce = priority 50).
      # In stage2 the ghc source comes from configuredSrc (GHC 9.14), not
      # the boot GHC (9.10.3).  mkOverride 49 beats mkForce.
      packages.ghc.src = lib.mkOverride 49 (configuredSrc + "/compiler");

      # ── packages.ghc setup isolation ─────────────────────────────────────
      # The custom Setup.hs imports Data.Map (containers).  In the stage2
      # plan, containers resolves to the local GHC 9.14 version which
      # transitively depends on ghc-internal-9.1400.0.  The boot GHC
      # (9.10.3) compiles Setup.hs and panics when it encounters
      # ghc-internal-9.1400.0 in the setup's package DB ("Invalid platform
      # constants").
      #
      # Fix: clear setup depends so the setup's package DB only contains
      # packages from the boot GHC's package DB (via nonReinstallablePkgs).
      # The boot GHC has containers, Cabal, base, directory, filepath,
      # process — everything the custom Setup.hs needs.
      packages.ghc.components.setup.depends = lib.mkForce [];
      packages.ghc.package.setup-depends = lib.mkForce [];
      packages.ghc-boot.components.setup.depends = lib.mkForce [];
      packages.ghc-boot.package.setup-depends = lib.mkForce [];

      # setup-builder.nix compiles Setup.hs directly with `ghc --make`,
      # NOT via Cabal, so the MIN_VERSION_* macros are not auto-generated.
      # GHC 9.14's Setup.hs uses #if MIN_VERSION_Cabal(3,14,0) etc.
      # Insert the macro definition AFTER language pragmas (GHC scans
      # pragmas first, then runs CPP) but BEFORE the first #if directive.
      packages.ghc.components.setup.preBuild = ''
        if [ -f Setup.hs ]; then
          awk '/^#if/ && !ins {print "#define MIN_VERSION_Cabal(a,b,c) ((a)<3||((a)==3&&((b)<12||((b)==12&&(c)<=1))))"; ins=1} {print}' Setup.hs > Setup.hs.tmp
          mv Setup.hs.tmp Setup.hs
        fi
      '';

      # GHC's compiler package has build-type: Custom.  The custom Setup.hs
      # generates primop-*.hs-incl, GHC/Platform/Constants.hs, and
      # GHC/Settings/Config.hs in its postConf hook.  It calls
      # genprimopcode and deriveConstants from PATH.  Prepend the in-tree
      # (9.14) versions so they shadow any boot-installed copies (same
      # approach as stage1).
      packages.ghc.preConfigure = ''
        export PATH=${genPrimopCodeTool}/bin:${deriveConstantsTool}/bin:$PATH
      '';

      # compiler/Setup.hs calls alex and happy to generate parser/lexer source
      # (needed when +build-tool-depends flag is set on the ghc library).
      # Although cabalProjectLocal re-enables Hackage so cabal can resolve
      # build-tool-depends automatically, we also add them explicitly here as a
      # belt-and-braces measure.
      packages.ghc.components.library.build-tools = [
        alexTool
        happyTool
      ];

      # GHC 9.14 checks for all rts sub-library variants (nonthreaded-nodebug
      # etc.) in the package DB even for C executables built with
      # -no-auto-link-packages.  unlit only depends on rts-fs, so the rts
      # sub-libraries are not present → GHC aborts.  -no-rts bypasses the
      # check (as suggested by the error message itself).
      packages.unlit.components.exes.unlit.ghcOptions = ["-no-rts"];

      # GHC 9.14's ghc-pkg reads a settings file at startup (for ABI hash
      # validation etc.).  It computes the settings path from a compiled-in
      # topdir — on Unix, getBaseDir returns Nothing so the topdir is
      # hardcoded at build time to the derivation output, i.e.
      # $out/lib/settings.  Without a settings file, ANY ghc-pkg invocation
      # (including `recache`) fails with:
      #   "Settings file doesn't exist: <ghc-pkg-derivation>/lib/settings"
      # Provide the settings file in the ghc-pkg derivation output via
      # postInstall so that we can use the raw binary for `recache` when
      # assembling the final sghc914Compiler.
      # GHC 9.14's ghc and ghc-pkg binaries read a settings file at startup.
      # On Unix, getBaseDir returns Nothing so the topdir is the compiled-in
      # derivation output path ($out/lib/settings).  Without it, ANY invocation
      # fails with "Settings file doesn't exist".
      # Provide the settings file in each exe derivation via postInstall.
      packages.ghc-pkg.components.exes.ghc-pkg.postInstall = ''
        mkdir -p $out/lib
        cp ${stage1Compiler}/lib/ghc-${ghcVersionFull}/settings $out/lib/settings
      '';
      packages.ghc-bin.components.exes.ghc.postInstall = ''
        mkdir -p $out/lib
        cp ${stage1Compiler}/lib/ghc-${ghcVersionFull}/settings $out/lib/settings
      '';
    }
    # GHC 9.14's unit resolver checks that all dependencies listed in .conf
    # files are present in the package DB.  When a package's per-component DB
    # contains ghc-internal.conf, GHC follows its depends field and expects to
    # find rts:nonthreaded-nodebug (baked in by GHC 9.14's unit wiring).  If
    # it is absent, GHC fails with:
    #   <command line>: unknown unit: rts:nonthreaded-nodebug (dependency of ghc-internal)
    #
    # Fix: set `additional-prebuilt-depends` at the PACKAGE level (not
    # components.library) so it applies to ALL component types (library,
    # exes, tests, etc.) via the package → component option inheritance.
    #
    # Excluded packages (to avoid circular dependency through
    # rts:nonthreaded-nodebug's own dependency chain):
    #   rts, libffi-clib, rts-fs, rts-headers  — deps of rts:nonthreaded-nodebug
    #
    # The three other rts sub-libs (threaded-nodebug, nonthreaded-debug,
    # threaded-debug) have `build-depends: rts` and are handled via
    # cabalProjectLocal with `ghc-options: -no-rts` (which bypasses the
    # mkUnitState wiring check).
    ({ config, lib, ... }: let
        rtsSublibs = with config.hsPkgs.rts.components.sublibs; [
          nonthreaded-nodebug
          threaded-nodebug
          nonthreaded-debug
          threaded-debug
        ];
        # Every package that (transitively) depends on base has
        # ghc-internal.conf in its per-component package DB.
        # ghc-internal.conf depends on ALL rts sub-libraries via GHC 9.14's
        # unit wiring.  We must add them to the package DB of every package.
        #
        # We cannot use `builtins.attrNames config.packages` here because
        # that creates infinite recursion (config.packages depends on these
        # settings).  Use a static list instead.
        #
        # Excluded (rts:nonthreaded-nodebug's own dep closure):
        #   rts, libffi-clib, rts-fs, rts-headers
        pkgsNeedingRts = [
          # Core boot libraries:
          "ghc-prim" "ghc-bignum" "ghc-internal" "base" "array" "binary"
          "bytestring" "containers" "deepseq" "exceptions" "filepath"
          "ghc-boot-th" "ghc-compact" "ghc-experimental" "ghc-heap"
          "hpc" "integer-gmp" "mtl" "os-string" "parsec" "pretty"
          "process" "semaphore-compat" "stm" "template-haskell" "text"
          "time" "transformers" "unix" "xhtml" "terminfo" "directory"
          "file-io" "haskeline"
          # GHC compiler packages:
          "ghc" "ghc-boot" "ghc-boot-th-next" "ghc-platform" "ghci"
          "ghc-toolchain" "Cabal" "Cabal-syntax"
          "haddock-api" "haddock-library"
          # Exe packages:
          "ghc-bin" "ghc-pkg" "hsc2hs" "runghc" "unlit"
          "ghc-toolchain-bin" "haddock"
          "genprimopcode" "deriveConstants"
          # Hackage build-tool deps (also rebuilt in stage2):
          "happy-lib" "happy" "alex"
        ];
      in {
        # Package-level: inherited by ALL component types via package → component
        # option inheritance (package.nix passes options via lib.mkDefault).
        # Filter to only include packages actually present in the plan.
        packages = lib.genAttrs
          (lib.filter (name: config.packages ? ${name}) pkgsNeedingRts)
          (_: { additional-prebuilt-depends = rtsSublibs; });
    })
    {

      # The rts package's install phase invokes GHC (ghcLink = LinkExecutable
      # Dynamic), which would panic in GHC 9.14's mkUnitState because the rts
      # sub-libraries (rts:nonthreaded-nodebug etc.) are not yet registered.
      # The primary fix is -no-rts in the stage1 wrapper (see stage1Compiler
      # above).  These two settings are additional mitigations that suppress
      # the GHC invocations that trigger the panic:
      #
      # 1. enableLibraryForGhci = false: sets the haskell.nix component
      #    parameter directly so comp-builder.nix emits --disable-library-for-ghci
      #    (without also emitting --enable-library-for-ghci).  This prevents
      #    Cabal from calling GHC to create the GHCi merged .o file.
      #
      # 2. configureFlags = ["--disable-shared"]: prevents Cabal from invoking
      #    GHC to build the rts shared library (.dylib).  The rts shared
      #    library is not needed for a functional stage2 compiler.
      packages.rts.components.library.enableLibraryForGhci = false;
      packages.rts.components.library.configureFlags = ["--disable-shared"];
      packages.rts.components.library.ghcOptions = ["-no-rts"];
    }];
  };

  s2 = stage2Project.hsPkgs;

  # Convenience: path to a stage2 executable.
  s2exe = pkg: exe: s2.${pkg}.components.exes.${exe}.exePath;

  # Stage2 boot library packages that need to be registered in the final
  # compiler's package DB so that `ghc-pkg list` shows them to cabal when
  # users build projects with sghc914.
  bootLibraries = [
    "array"        "base"          "binary"         "bytestring"
    "Cabal"        "Cabal-syntax"  "containers"     "deepseq"
    "directory"    "exceptions"    "file-io"        "filepath"
    "ghc"          "ghc-bignum"    "ghc-boot"       "ghc-boot-th"
    "ghc-compact"  "ghc-experimental"               "ghc-heap"
    "ghc-internal" "ghc-platform"  "ghc-prim"       "ghc-toolchain"
    "ghci"         "haddock-api"   "haddock-library" "haskeline"
    "hpc"          "integer-gmp"   "libffi-clib"    "mtl"
    "os-string"    "parsec"        "pretty"         "process"
    "rts"          "rts-fs"        "rts-headers"    "semaphore-compat"
    "stm"          "template-haskell"               "text"
    "time"         "transformers"  "unix"           "xhtml"
  ];

  # For each boot library, the path to its registered .conf file(s).
  # We check `s2 ? name` first to handle any packages missing from the plan.
  bootLibConfs = lib.concatMap (name:
    let
      hasPkg  = s2 ? ${name};
      hasLib  = hasPkg && (s2.${name}.components ? library);
      libDrv  = lib.optionalString hasLib "${s2.${name}.components.library}/package.conf.d";
    in lib.optional hasLib libDrv
  ) bootLibraries
  # Include rts sub-libraries so the final compiler's package DB contains
  # rts:nonthreaded-nodebug etc.  These are needed for GHC 9.14's mkUnitState
  # wiring when users compile projects with sghc914.
  ++ lib.optionals (s2 ? rts && s2.rts.components ? sublibs) (
    lib.concatMap (sublibName:
      let hasSub = s2.rts.components.sublibs ? ${sublibName};
      in lib.optional hasSub
           "${s2.rts.components.sublibs.${sublibName}}/package.conf.d"
    ) [ "nonthreaded-nodebug" "threaded-nodebug" "nonthreaded-debug" "threaded-debug" ]
  );

  # ── Final sghc914 compiler ────────────────────────────────────────────────
  sghc914Compiler =
    let rawDrv = pkgs.runCommand "sghc914-compiler" {
      passthru = {
        version              = ghcVersionFull;
        targetPrefix         = "";
        haskellCompilerName  = "ghc-${ghcVersionFull}";
        isHaskellNixCompiler = true;
        libDir               = "lib/ghc-${ghcVersionFull}";
        enableShared         = !pkgs.stdenv.hostPlatform.isMusl && !pkgs.stdenv.hostPlatform.isStatic;
        # call-cabal-project-to-nix.nix:297 needs raw-src to locate bundled
        # package .cabal files.  Our configuredSrc is the fully-patched GHC
        # source tree with all .cabal files generated, which is exactly what
        # is needed.  raw-src is a function of evalPackages (ignored here).
        raw-src              = _: configuredSrc;
        # override references the outer sghc914Compiler (with cachedDeps).
        override             = _: sghc914Compiler;
      };
    } ''
    mkdir -p $out/bin
    mkdir -p $out/lib/ghc-${ghcVersionFull}

    # ── Executables ──────────────────────────────────────────────────────────
    cat > $out/bin/ghc << 'ENDSCRIPT'
#!/bin/sh
exec GHC_EXE \
  -no-global-package-db \
  -package-db PACKAGEDB \
  "$@"
ENDSCRIPT
    sed -i \
      -e "s|GHC_EXE|${s2exe "ghc-bin" "ghc"}|" \
      -e "s|PACKAGEDB|$out/lib/ghc-${ghcVersionFull}/package.conf.d|" \
      $out/bin/ghc
    chmod +x $out/bin/ghc

    ln -s $out/bin/ghc $out/bin/ghci

    # ── ghc-pkg wrapper ───────────────────────────────────────────────────────
    # Wrap ghc-pkg to always use our populated package DB.  This lets
    # makeCompilerDeps (haskell.nix's cachedDeps mechanism) enumerate the
    # boot libraries by running `ghc-pkg list` with the right package DB.
    cat > $out/bin/ghc-pkg << 'ENDSCRIPT'
#!/bin/sh
exec GHC_PKG_EXE \
  --no-user-package-db \
  --global-package-db PACKAGEDB \
  "$@"
ENDSCRIPT
    sed -i \
      -e "s|GHC_PKG_EXE|${s2exe "ghc-pkg" "ghc-pkg"}|" \
      -e "s|PACKAGEDB|$out/lib/ghc-${ghcVersionFull}/package.conf.d|" \
      $out/bin/ghc-pkg
    chmod +x $out/bin/ghc-pkg

    ln -sf ${s2exe "hsc2hs"  "hsc2hs"}  $out/bin/hsc2hs
    ln -sf ${s2exe "runghc"  "runghc"}  $out/bin/runghc
    ln -sf ${s2exe "unlit"   "unlit"}   $out/bin/unlit
    # GHC 9.14 looks for unlit at $topdir/../bin/unlit where topdir = lib/ghc-9.14.0
    # (set via -B$NIX_GHC_LIBDIR in ghc-for-component-wrapper.nix).
    # lib/ghc-9.14.0/../bin/ resolves to lib/bin/, so unlit must be there.
    # ghc-for-component-wrapper lndir's the compiler then re-links lib/bin/
    # if it exists, so we provide lib/bin/{unlit,hsc2hs} here.
    mkdir -p $out/lib/bin
    ln -sf ${s2exe "unlit"   "unlit"}   $out/lib/bin/unlit
    ln -sf ${s2exe "hsc2hs"  "hsc2hs"} $out/lib/bin/hsc2hs

    # ── settings + support files ─────────────────────────────────────────────
    # Copy settings BEFORE ghc-pkg init/recache — the stage2 ghc-pkg binary
    # (built with GHC 9.14) expects a settings file at $libdir/settings.
    cp ${stage1Compiler}/lib/ghc-${ghcVersionFull}/settings \
       $out/lib/ghc-${ghcVersionFull}/settings

    # ── Package DB ───────────────────────────────────────────────────────────
    # Populate package DB with .conf files from all stage2-built boot
    # libraries, then recache.
    #
    # The stage2 ghc-pkg binary has a settings file at its derivation
    # output ($out/lib/settings) thanks to the postInstall hook above.
    # Use it directly with --global-package-db to point at our output DB.
    mkdir -p $out/lib/ghc-${ghcVersionFull}/package.conf.d

    for confDir in ${lib.concatStringsSep " " bootLibConfs}; do
      cp "$confDir"/*.conf \
         $out/lib/ghc-${ghcVersionFull}/package.conf.d/ 2>/dev/null || true
    done

    ${s2exe "ghc-pkg" "ghc-pkg"} recache \
      --no-user-package-db \
      --global-package-db $out/lib/ghc-${ghcVersionFull}/package.conf.d
    cp -r ${stage1Compiler}/lib/ghc-${ghcVersionFull}/include \
          $out/lib/ghc-${ghcVersionFull}/include
    cp ${configuredSrc}/utils/hsc2hs/data/template-hsc.h \
       $out/lib/ghc-${ghcVersionFull}/template-hsc.h
    cp ${configuredSrc}/driver/ghc-usage.txt  $out/lib/ghc-${ghcVersionFull}/
    cp ${configuredSrc}/driver/ghci-usage.txt $out/lib/ghc-${ghcVersionFull}/
    '';
    in pkgs.haskell-nix.haskellLib.makeCompilerDeps rawDrv;

  # ═══════════════════════════════════════════════════════════════════════════
  # ── Cross-compilation support (isCrossTarget) ──────────────────────────────
  # Active when targetPlatform != hostPlatform, i.e. in
  # pkgsCross.wasi32.buildPackages (host=native, target=wasm32).
  #
  # Uses the native sghc914 (compiler-nix-name = "sghc914") to build boot
  # libraries via buildPackages.haskell-nix.cabalProject'.  The cross compiler
  # is a single assembly: native sghc914 binaries + target-platform settings
  # (from ghc-toolchain-bin) + boot library package DB.
  # ═══════════════════════════════════════════════════════════════════════════

  # The native sghc914 compiler, already built for the build platform.
  # In case 2, buildPackages = (native, native, native) = case 1.
  nativeSghc914       = pkgs.buildPackages.haskell-nix.compiler.sghc914;
  nativeStage1        = pkgs.buildPackages.haskell-nix.compiler."sghc914-stage1";
  nativeConfiguredSrc = nativeSghc914.raw-src {};

  # ghc-toolchain-bin from the native stage1 build (runs natively).
  nativeGhcToolchainBin =
    nativeStage1.project.hsPkgs.ghc-toolchain-bin.components.exes.ghc-toolchain-bin.exePath;

  # Target C compiler and bintools — runs natively, targets wasm32.
  # pkgs.targetPackages in case 2 = pkgsCross.wasi32.
  targetCC       = pkgs.targetPackages.stdenv.cc;
  targetBintools = targetCC.bintools.bintools;

  # ── Native build tools for cross ──────────────────────────────────────────
  # Build tools must run natively.  Use buildPackages.haskell-nix.tool so
  # they are compiled for the build platform, not the target.
  crossAlexTool  = pkgs.buildPackages.haskell-nix.tool bootGhcName "alex"  {
    version = "3.5.2.0"; evalPackages = pkgs.buildPackages;
  };
  crossHappyTool = pkgs.buildPackages.haskell-nix.tool bootGhcName "happy" {
    version = "2.1.5";   evalPackages = pkgs.buildPackages;
  };
  crossGenPrimopCodeTool = pkgs.buildPackages.haskell-nix.tool bootGhcName "genprimopcode" {
    src = "${nativeConfiguredSrc}/utils/genprimopcode";
    evalPackages = pkgs.buildPackages;
  };
  crossDeriveConstantsTool = pkgs.buildPackages.haskell-nix.tool bootGhcName "deriveConstants" {
    src = "${nativeConfiguredSrc}/utils/deriveConstants";
    evalPackages = pkgs.buildPackages;
  };

  # Convenience: path to a native stage1 executable (for cross tools).
  cs1exe = pkg: exe:
    nativeStage1.project.hsPkgs.${pkg}.components.exes.${exe}.exePath;

  # ── Cross boot library project ──────────────────────────────────────────
  # Builds boot libraries using the native sghc914-stage1 compiler.
  # stage1 has a minimal package DB, avoiding conflicts when rebuilding
  # boot packages from source.
  crossBootProject = pkgs.buildPackages.haskell-nix.cabalProject' {
    name                 = "sghc914-cross";
    src                  = nativeConfiguredSrc;
    cabalProjectFileName = "cabal.project.stage2.merged";
    compiler-nix-name    = "sghc914-stage1";
    evalPackages         = pkgs.buildPackages;
    configureArgs = "--disable-tests --disable-benchmarks";

    cabalProjectLocal = ''
      active-repositories: hackage.haskell.org

      package rts
        ghc-options: -no-rts

      package rts-fs
        ghc-options: -no-rts

      package rts-headers
        ghc-options: -no-rts
    '';

    modules = [{
      nonReinstallablePkgs = [
        "rts" "ghc-prim" "integer-gmp" "integer-simple" "base"
        "ghc-bignum" "system-cxx-std-lib" "ghc-internal"
        "ghc-boot" "ghc" "Cabal" "array" "binary" "bytestring"
        "containers" "deepseq" "directory" "filepath" "ghc-boot-th"
        "ghc-compact" "ghc-heap" "hpc" "mtl" "parsec" "pretty"
        "process" "stm" "exceptions" "semaphore-compat" "os-string"
        "template-haskell" "text" "time" "transformers" "unix"
        "xhtml" "terminfo"
      ];

      # ── libffi-clib ──────────────────────────────────────────────────────
      packages.libffi-clib.preConfigure = ''
        ./configure 2>&1 | tail -3 || true
        fficonfig=$(find . -maxdepth 2 -name fficonfig.h 2>/dev/null | head -1)
        [ -n "$fficonfig" ] && cp "$fficonfig" include/fficonfig.h
        build_dir=$(dirname "$fficonfig" 2>/dev/null)
        [ -f "$build_dir/include/ffi.h" ] && cp "$build_dir/include/ffi.h" include/ffi.h
        [ -f "$build_dir/libffi-clib.buildinfo" ] && cp "$build_dir/libffi-clib.buildinfo" libffi-clib.buildinfo
      '';

      # ── Configure aux files ──────────────────────────────────────────────
      packages.ghc-internal.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${nativeConfiguredSrc}/$f ] && cp ${nativeConfiguredSrc}/$f ../ || true
        done
        mkdir -p include
        cp -f ${nativeConfiguredSrc}/libraries/ghc-internal/include/*.h include/ 2>/dev/null || true
        export CPPFLAGS="-I$(pwd) ''${CPPFLAGS:-}"
        echo 'ac_cv_sizeof_struct_MD5Context=88' > config.cache
      '';
      packages.unix.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${nativeConfiguredSrc}/$f ] && cp ${nativeConfiguredSrc}/$f ../ || true
        done
      '';
      packages.directory.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${nativeConfiguredSrc}/$f ] && cp ${nativeConfiguredSrc}/$f ../ || true
        done
      '';
      packages.process.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${nativeConfiguredSrc}/$f ] && cp ${nativeConfiguredSrc}/$f ../ || true
        done
      '';
      packages.time.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${nativeConfiguredSrc}/$f ] && cp ${nativeConfiguredSrc}/$f ../ || true
        done
      '';
      packages.terminfo.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${nativeConfiguredSrc}/$f ] && cp ${nativeConfiguredSrc}/$f ../ || true
        done
      '';

      # ── rts ──────────────────────────────────────────────────────────────
      # NM and OBJDUMP use the TARGET bintools (wasm32) so they can
      # process target object files.  deriveConstants and genapply run
      # natively.
      packages.rts.preConfigure = ''
        for f in install-sh config.sub config.guess depcomp mkinstalldirs; do
          [ -f ${nativeConfiguredSrc}/$f ] && cp ${nativeConfiguredSrc}/$f ../ || true
        done
        cp ${nativeConfiguredSrc}/rts/configure .
        chmod +x configure
        export DERIVE_CONSTANTS=${crossDeriveConstantsTool}/bin/deriveConstants
        export GENAPPLY=${cs1exe "genapply" "genapply"}
        export NM=${targetCC.bintools}/bin/${targetCC.bintools.targetPrefix}nm
        export OBJDUMP=${targetCC.bintools}/bin/${targetCC.bintools.targetPrefix}objdump
        export PYTHON=${pkgs.buildPackages.python3}/bin/python3
        export CABAL_FLAG_libdw=0
        export CABAL_FLAG_libbfd=0
        export CABAL_FLAG_libzstd=0
        export CABAL_FLAG_static_libzstd=0
        export CABAL_FLAG_libnuma=0
        export CABAL_FLAG_unregisterised=0
        export CABAL_FLAG_tables_next_to_code=${if tablesNextToCode == "YES" then "1" else "0"}
        export CABAL_FLAG_leading_underscore=${if tp.isDarwin or false then "1" else "0"}
        ./configure
      '';

      # ── ghc-boot ─────────────────────────────────────────────────────────
      packages.ghc-boot.preConfigure = ''
        export GIT_COMMIT_ID="0000000000000000000000000000000000000000"
      '';

      # ── packages.ghc source + setup isolation ────────────────────────────
      packages.ghc.src = lib.mkOverride 49 (nativeConfiguredSrc + "/compiler");
      packages.ghc.components.setup.depends = lib.mkForce [];
      packages.ghc.package.setup-depends = lib.mkForce [];
      packages.ghc-boot.components.setup.depends = lib.mkForce [];
      packages.ghc-boot.package.setup-depends = lib.mkForce [];

      packages.ghc.components.setup.preBuild = ''
        if [ -f Setup.hs ]; then
          awk '/^#if/ && !ins {print "#define MIN_VERSION_Cabal(a,b,c) ((a)<3||((a)==3&&((b)<12||((b)==12&&(c)<=1))))"; ins=1} {print}' Setup.hs > Setup.hs.tmp
          mv Setup.hs.tmp Setup.hs
        fi
      '';

      packages.ghc.preConfigure = ''
        export PATH=${crossGenPrimopCodeTool}/bin:${crossDeriveConstantsTool}/bin:$PATH
      '';

      packages.unlit.components.exes.unlit.ghcOptions = ["-no-rts"];
      packages.rts-fs.components.library.ghcOptions = ["-no-rts"];
      packages.rts-headers.components.library.ghcOptions = ["-no-rts"];
    }
    # ── Force native build-tools for all components ────────────────────────
    # build-tools is additive by default (listOfFilteringNulls), so without
    # mkForce the plan's build-tool-depends (happy/alex from the project)
    # are also included, causing link failures against the incomplete RTS.
    ({ config, lib, ... }: {
      packages = lib.genAttrs
        (lib.filter (name: config.packages ? ${name})
          ["ghc" "Cabal-syntax" "Cabal" "haddock-api" "haddock-library"
           "ghci" "ghc-toolchain" "ghc-toolchain-bin" "template-haskell"
           "ghc-boot" "ghc-boot-th-next"])
        (_: {
          components.library.build-tools = lib.mkForce [
            crossAlexTool crossHappyTool
          ];
        });
    })
    # ── rts sub-library additional-prebuilt-depends ────────────────────────
    ({ config, lib, ... }: let
        rtsSublibs = with config.hsPkgs.rts.components.sublibs; [
          nonthreaded-nodebug
          threaded-nodebug
          nonthreaded-debug
          threaded-debug
        ];
        pkgsNeedingRts = [
          "ghc-prim" "ghc-bignum" "ghc-internal" "base" "array" "binary"
          "bytestring" "containers" "deepseq" "exceptions" "filepath"
          "ghc-boot-th" "ghc-compact" "ghc-experimental" "ghc-heap"
          "hpc" "integer-gmp" "mtl" "os-string" "parsec" "pretty"
          "process" "semaphore-compat" "stm" "template-haskell" "text"
          "time" "transformers" "unix" "xhtml" "terminfo" "directory"
          "file-io" "haskeline"
          "ghc" "ghc-boot" "ghc-boot-th-next" "ghc-platform" "ghci"
          "ghc-toolchain" "Cabal" "Cabal-syntax"
          "haddock-api" "haddock-library"
          "ghc-bin" "ghc-pkg" "hsc2hs" "runghc" "unlit"
          "ghc-toolchain-bin" "haddock"
          "genprimopcode" "deriveConstants"
          "happy-lib" "happy" "alex"
        ];
      in {
        packages = lib.genAttrs
          (lib.filter (name: config.packages ? ${name}) pkgsNeedingRts)
          (_: { additional-prebuilt-depends = rtsSublibs; });
    })
    {
      packages.rts.components.library.enableLibraryForGhci = false;
      packages.rts.components.library.configureFlags = ["--disable-shared"];
      packages.rts.components.library.ghcOptions = ["-no-rts"];
    }];
  };

  cs2 = crossBootProject.hsPkgs;

  # Cross boot libraries — same list as native.
  crossBootLibConfs = lib.concatMap (name:
    let
      hasPkg  = cs2 ? ${name};
      hasLib  = hasPkg && (cs2.${name}.components ? library);
      libDrv  = lib.optionalString hasLib "${cs2.${name}.components.library}/package.conf.d";
    in lib.optional hasLib libDrv
  ) bootLibraries
  ++ lib.optionals (cs2 ? rts && cs2.rts.components ? sublibs) (
    lib.concatMap (sublibName:
      let hasSub = cs2.rts.components.sublibs ? ${sublibName};
      in lib.optional hasSub
           "${cs2.rts.components.sublibs.${sublibName}}/package.conf.d"
    ) [ "nonthreaded-nodebug" "threaded-nodebug" "nonthreaded-debug" "threaded-debug" ]
  );

  # ── Cross compiler ──────────────────────────────────────────────────────
  # Single assembly: native sghc914 binaries + target settings + boot libs.
  crossCompiler =
    let rawDrv = pkgs.buildPackages.runCommand "sghc914-cross-compiler" {
      passthru = {
        version              = ghcVersionFull;
        targetPrefix         = "";
        haskellCompilerName  = "ghc-${ghcVersionFull}";
        isHaskellNixCompiler = true;
        libDir               = "lib/ghc-${ghcVersionFull}";
        enableShared         = tp.isWasm or false;
        raw-src              = _: nativeConfiguredSrc;
        override             = _: crossCompiler;
      };
      nativeBuildInputs = [ targetCC targetBintools ];
    } ''
    mkdir -p $out/bin
    mkdir -p $out/lib/ghc-${ghcVersionFull}

    # ── ghc wrapper ──────────────────────────────────────────────────────
    cat > $out/bin/ghc << 'ENDSCRIPT'
#!/bin/sh
exec GHC_EXE \
  -no-global-package-db \
  -package-db PACKAGEDB \
  -BLIBDIR \
  "$@"
ENDSCRIPT
    sed -i \
      -e "s|GHC_EXE|${nativeSghc914}/bin/ghc|" \
      -e "s|PACKAGEDB|$out/lib/ghc-${ghcVersionFull}/package.conf.d|" \
      -e "s|LIBDIR|$out/lib/ghc-${ghcVersionFull}|" \
      $out/bin/ghc
    chmod +x $out/bin/ghc

    ln -s $out/bin/ghc $out/bin/ghci

    # ── ghc-pkg wrapper ──────────────────────────────────────────────────
    cat > $out/bin/ghc-pkg << 'ENDSCRIPT'
#!/bin/sh
exec GHC_PKG_EXE \
  --no-user-package-db \
  --global-package-db PACKAGEDB \
  "$@"
ENDSCRIPT
    sed -i \
      -e "s|GHC_PKG_EXE|${nativeSghc914}/bin/ghc-pkg|" \
      -e "s|PACKAGEDB|$out/lib/ghc-${ghcVersionFull}/package.conf.d|" \
      $out/bin/ghc-pkg
    chmod +x $out/bin/ghc-pkg

    ln -sf ${nativeSghc914}/bin/hsc2hs  $out/bin/hsc2hs
    ln -sf ${nativeSghc914}/bin/runghc  $out/bin/runghc
    ln -sf ${nativeSghc914}/bin/unlit   $out/bin/unlit
    mkdir -p $out/lib/bin
    ln -sf ${nativeSghc914}/bin/unlit   $out/lib/bin/unlit
    ln -sf ${nativeSghc914}/bin/hsc2hs  $out/lib/bin/hsc2hs

    # ── Target-platform settings ─────────────────────────────────────────
    cp ${nativeConfiguredSrc}/config.sub ./config.sub
    ${nativeGhcToolchainBin} \
      --disable-ld-override \
      --triple ${tp.config} \
      --cc  ${targetCC}/bin/${targetCC.targetPrefix}cc \
      --cxx ${targetCC}/bin/${targetCC.targetPrefix}c++ \
      --output-settings \
      -o $out/lib/ghc-${ghcVersionFull}/settings

    # ── ghcversion.h ─────────────────────────────────────────────────────
    mkdir -p $out/lib/ghc-${ghcVersionFull}/include
    cp ${nativeConfiguredSrc}/rts/include/ghcversion.h \
       $out/lib/ghc-${ghcVersionFull}/include/

    # ── Package DB (boot libraries) ──────────────────────────────────────
    mkdir -p $out/lib/ghc-${ghcVersionFull}/package.conf.d

    for confDir in ${lib.concatStringsSep " " crossBootLibConfs}; do
      cp "$confDir"/*.conf \
         $out/lib/ghc-${ghcVersionFull}/package.conf.d/ 2>/dev/null || true
    done

    ${nativeSghc914}/bin/ghc-pkg recache \
      --no-user-package-db \
      --global-package-db $out/lib/ghc-${ghcVersionFull}/package.conf.d

    # ── Support files ────────────────────────────────────────────────────
    cp ${nativeConfiguredSrc}/utils/hsc2hs/data/template-hsc.h \
       $out/lib/ghc-${ghcVersionFull}/template-hsc.h
    cp ${nativeConfiguredSrc}/driver/ghc-usage.txt  $out/lib/ghc-${ghcVersionFull}/
    cp ${nativeConfiguredSrc}/driver/ghci-usage.txt $out/lib/ghc-${ghcVersionFull}/

    # ── JSFFI runtime (.mjs) ──────────────────────────────────────────────
    # These scripts are needed by the wasm backend for JavaScript interop.
    for f in ${nativeConfiguredSrc}/utils/jsffi/*.mjs; do
      [ -e "$f" ] && cp "$f" $out/lib/
    done
    '';
    in pkgs.haskell-nix.haskellLib.makeCompilerDeps rawDrv;

in {
  haskell-nix = prev.haskell-nix // {
    compiler = prev.haskell-nix.compiler // (
      # Case 2: isCrossTarget (e.g. pkgsCross.wasi32.buildPackages)
      #   sghc914 = cross compiler with target boot libraries
      if isCrossTarget then {
        sghc914 = crossCompiler;
      }
      # Case 3: isCrossHost (e.g. pkgsCross.wasi32)
      #   → can't build a compiler that runs on wasm32; skip
      else if isCrossHost then {}
      # Case 1: native
      else {
        "sghc914-stage1" = stage1Compiler;
        sghc914           = sghc914Compiler;
      }
    );
  };
}
