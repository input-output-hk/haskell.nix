# This is the nix expression to build ghc. The Glasgow Haskell Compiler. It is
# heavily inspired (and based) upon the ghc expression from nixos/nixpkgs.
# haskell.nix ships its own version of the ghc expression as it needs more
# control over the expression to isolate it against varying <nixpkgs> and
# allow us to customize it to the way haskell.nix works.
let self =
{ stdenv, lib, haskell-nix, targetPackages

# build-tools
, bootPkgs
, buildPackages
, autoconf, automake, coreutils, fetchurl, fetchpatch, perl, python3, m4, sphinx, numactl, elfutils
, autoreconfHook
, bash

, libiconv ? null, ncurses

, installDeps

, # GHC can be built with system libffi or a bundled one.
  libffi ? null

, useLLVM ? !stdenv.targetPlatform.isx86
, # LLVM is conceptually a run-time-only dependency, but for
  # non-x86, we need LLVM to bootstrap later stages, so it becomes a
  # build-time dependency too.
  buildLlvmPackages, llvmPackages

, # If enabled, GHC will be built with the GPL-free but slower integer-simple
  # library instead of the faster but GPLed integer-gmp library.
  enableIntegerSimple ? !(lib.any (lib.meta.platformMatch stdenv.hostPlatform) gmp.meta.platforms), gmp

, # If enabled, use -fPIC when compiling static libs.
  enableRelocatedStaticLibs ? stdenv.targetPlatform != stdenv.hostPlatform && !stdenv.targetPlatform.isAarch32

, # Whether to build dynamic libs for the standard library (on the target
  # platform). Static libs are always built.
  enableShared ? !haskell-nix.haskellLib.isCrossTarget

, enableLibraryProfiling ? true

, enableDWARF ? false

, # Whether to build terminfo.  Musl fails to build terminfo as ncurses seems to be linked to glibc
  enableTerminfo ? !stdenv.targetPlatform.isWindows && !stdenv.targetPlatform.isMusl

, # Wheter to build in NUMA support
  enableNUMA ? true

, # What flavour to build. An empty string indicates no
  # specific flavour and falls back to ghc default values.
  ghcFlavour ? lib.optionalString haskell-nix.haskellLib.isCrossTarget (
    if useLLVM
      then "perf-cross"
      else "perf-cross-ncg"
    )

, # Whether to disable the large address space allocator
  # necessary fix for iOS: https://www.reddit.com/r/haskell/comments/4ttdz1/building_an_osxi386_to_iosarm64_cross_compiler/d5qvd67/
  disableLargeAddressSpace ? stdenv.targetPlatform.isDarwin && stdenv.targetPlatform.isAarch64

, ghc-version ? src-spec.version
, ghc-version-date ? null
, src-spec
, ghc-patches ? []

# extra values we want to have available as passthru values.
, extra-passthru ? {}
}@args:

assert !enableIntegerSimple -> gmp != null;

let
  inherit (stdenv) buildPlatform hostPlatform targetPlatform;
  inherit (haskell-nix.haskellLib) isCrossTarget;

  inherit (bootPkgs) ghc;

  # TODO check if this possible fix for segfaults works or not.
  targetLibffi =
    # on native platforms targetPlatform.{libffi, gmp} do not exist; thus fall back
    # to the non-targetPlatform version in those cases.
    let targetLibffi = targetPackages.libffi or libffi; in
    # we need to set `dontDisableStatic` for musl for libffi to work.
    if stdenv.targetPlatform.isMusl
    then targetLibffi.overrideAttrs (old: { dontDisableStatic = true; })
    else targetLibffi;

  targetGmp = targetPackages.gmp or gmp;

  targetIconv = targetPackages.libiconv or libiconv;

  targetNumactl = targetPackages.numactl or numactl;

  # TODO(@Ericson2314) Make unconditional
  targetPrefix = lib.optionalString
    (targetPlatform != hostPlatform)
    "${targetPlatform.config}-";

  buildMK = ''
    BuildFlavour = ${ghcFlavour}
    ifneq \"\$(BuildFlavour)\" \"\"
    include mk/flavours/\$(BuildFlavour).mk
    endif
    DYNAMIC_GHC_PROGRAMS = ${if enableShared then "YES" else "NO"}
    INTEGER_LIBRARY = ${if enableIntegerSimple then "integer-simple" else "integer-gmp"}
    EXTRA_HADDOCK_OPTS += --quickjump --hyperlinked-source
  '' + lib.optionalString (targetPlatform != hostPlatform) ''
    CrossCompilePrefix = ${targetPrefix}
  '' + lib.optionalString isCrossTarget ''
    Stage1Only = ${if targetPlatform.system == hostPlatform.system then "NO" else "YES"}
  ''
    # GHC 9.0.1 fails to compile for musl unless HADDOC_DOCS = NO
    + lib.optionalString (isCrossTarget || (targetPlatform.isMusl && builtins.compareVersions ghc-version "9.0.1" >= 0)) ''
    HADDOCK_DOCS = NO
    BUILD_SPHINX_HTML = NO
    BUILD_SPHINX_PDF = NO
  '' + lib.optionalString enableRelocatedStaticLibs ''
    GhcLibHcOpts += -fPIC
    GhcRtsHcOpts += -fPIC
  '' + lib.optionalString enableDWARF ''
    GhcLibHcOpts += -g3
    GhcRtsHcOpts += -g3
  '' + lib.optionalString targetPlatform.useAndroidPrebuilt ''
    EXTRA_CC_OPTS += -std=gnu99
  '' + lib.optionalString (!enableTerminfo) ''
    WITH_TERMINFO=NO
  ''
  # musl doesn't have a system-linker. Only on x86, and on x86 we need it, as
  # our elf linker for x86_64 is broken.
  + lib.optionalString (targetPlatform.isAndroid || (targetPlatform.isMusl && !targetPlatform.isx86)) ''
    compiler_CONFIGURE_OPTS += --flags=-dynamic-system-linker
  ''
  # While split sections are now enabled by default in ghc 8.8 for windows,
  # the seem to lead to `too many sections` errors when building base for
  # profiling.
  #
  # It appears that loading split sections through iserv on qemu-aarch64, is
  # particularly slow. Let's disable them for now.
  + lib.optionalString (targetPlatform.isWindows || targetPlatform.isAndroid) ''
    SplitSections = NO
  '' + lib.optionalString (!enableLibraryProfiling) ''
    BUILD_PROF_LIBS = NO
  '';

  # Splicer will pull out correct variations
  libDeps = platform: lib.optional enableTerminfo [ ncurses ncurses.dev ]
    ++ [targetLibffi]
    ++ lib.optional (!enableIntegerSimple) gmp
    ++ lib.optional (platform.libc != "glibc" && !targetPlatform.isWindows) libiconv
    ++ lib.optional (enableNUMA && platform.isLinux && !platform.isAarch32 && !platform.isAndroid) numactl;

  toolsForTarget =
    if hostPlatform == buildPlatform then
      [ targetPackages.stdenv.cc ] ++ lib.optional useLLVM llvmPackages.llvm
    else assert targetPlatform == hostPlatform; # build != host == target
      [ stdenv.cc ] ++ lib.optional useLLVM buildLlvmPackages.llvm;

  targetCC = builtins.head toolsForTarget;

in
stdenv.mkDerivation (rec {
  version = ghc-version;
  name = "${targetPrefix}ghc-${version}";

  patches = ghc-patches;

  src = if src-spec ? file
    then src-spec.file
    else fetchurl { inherit (src-spec) url sha256; };

  # configure was run by configured-src already.
  phases = [ "unpackPhase" "patchPhase" ]
            ++ lib.optional (ghc-patches != []) "autoreconfPhase"
            ++ [ "configurePhase" "buildPhase"
             "checkPhase" "installPhase"
             "fixupPhase"
             "installCheckPhase"
             "distPhase"
             ];

  # GHC is a bit confused on its cross terminology.
  preConfigure =
    # This code is only included when cross compiling as it breaks aarch64-darwin native compilation
    lib.optionalString (targetPlatform != hostPlatform) ''
        for env in $(env | grep '^TARGET_' | sed -E 's|\+?=.*||'); do
        export "''${env#TARGET_}=''${!env}"
        done
        # GHC is a bit confused on its cross terminology, as these would normally be
        # the *host* tools.
        export CC="${targetCC}/bin/${targetCC.targetPrefix}cc"
        export CXX="${targetCC}/bin/${targetCC.targetPrefix}cxx"
        # Use gold to work around https://sourceware.org/bugzilla/show_bug.cgi?id=16177
        export LD="${targetCC.bintools}/bin/${targetCC.bintools.targetPrefix}ld${lib.optionalString targetPlatform.isAarch32 ".gold"}"
        export AS="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}as"
        export AR="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}ar"
        export NM="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}nm"
        export RANLIB="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}ranlib"
        export READELF="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}readelf"
        export STRIP="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}strip"
    '' + ''
        echo -n "${buildMK}" > mk/build.mk
        sed -i -e 's|-isysroot /Developer/SDKs/MacOSX10.5.sdk||' configure
    '' + lib.optionalString useLLVM ''
        export LLC="${llvmPackages.llvm}/bin/llc"
        export OPT="${llvmPackages.llvm}/bin/opt"
    '' + lib.optionalString (!stdenv.isDarwin) ''
        export NIX_LDFLAGS+=" -rpath $out/lib/${targetPrefix}ghc-${ghc-version}"
    '' + lib.optionalString stdenv.isDarwin ''
        export NIX_LDFLAGS+=" -no_dtrace_dof"
    '' + lib.optionalString targetPlatform.useAndroidPrebuilt ''
        sed -i -e '5i ,("armv7a-unknown-linux-androideabi", ("e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64", "cortex-a8", ""))' llvm-targets
    '' + lib.optionalString targetPlatform.isMusl ''
        echo "patching llvm-targets for musl targets..."
        echo "Cloning these existing '*-linux-gnu*' targets:"
        grep linux-gnu llvm-targets | sed 's/^/  /'
        echo "(go go gadget sed)"
        sed -i 's,\(^.*linux-\)gnu\(.*\)$,\0\n\1musl\2,' llvm-targets
        echo "llvm-targets now contains these '*-linux-musl*' targets:"
        grep linux-musl llvm-targets | sed 's/^/  /'

        echo "And now patching to preserve '-musleabi' as done with '-gnueabi'"
        # (aclocal.m4 is actual source, but patch configure as well since we don't re-gen)
        for x in configure aclocal.m4; do
            substituteInPlace $x \
            --replace '*-android*|*-gnueabi*)' \
                        '*-android*|*-gnueabi*|*-musleabi*)'
        done
    '' + lib.optionalString (src-spec.version != ghc-version) ''
        substituteInPlace configure --replace 'RELEASE=YES' 'RELEASE=NO'
        echo '${ghc-version}' > VERSION
    '' + lib.optionalString (ghc-version-date != null) ''
        substituteInPlace configure --replace 'RELEASE=YES' 'RELEASE=NO'
        echo '${ghc-version-date}' > VERSION_DATE
    '';

  configurePlatforms = [ "build" "host" "target" ];
  # `--with` flags for libraries needed for RTS linker
  configureFlags = [
        "--datadir=$doc/share/doc/ghc"
        "--with-curses-includes=${ncurses.dev}/include" "--with-curses-libraries=${ncurses.out}/lib"
    ] ++ lib.optionals (targetLibffi != null) ["--with-system-libffi" "--with-ffi-includes=${targetLibffi.dev}/include" "--with-ffi-libraries=${targetLibffi.out}/lib"
    ] ++ lib.optional (!enableIntegerSimple) [
        "--with-gmp-includes=${targetGmp.dev}/include" "--with-gmp-libraries=${targetGmp.out}/lib"
    ] ++ lib.optional (targetPlatform == hostPlatform && hostPlatform.libc != "glibc" && !targetPlatform.isWindows) [
        "--with-iconv-includes=${libiconv}/include" "--with-iconv-libraries=${libiconv}/lib"
    ] ++ lib.optional (targetPlatform != hostPlatform) [
        "--with-iconv-includes=${targetIconv}/include" "--with-iconv-libraries=${targetIconv}/lib"
    ] ++ lib.optionals (targetPlatform != hostPlatform) [
        "--enable-bootstrap-with-devel-snapshot"
    ] ++ lib.optionals (disableLargeAddressSpace) [
        "--disable-large-address-space"
    ] ++ lib.optionals (targetPlatform.isAarch32) [
        "CFLAGS=-fuse-ld=gold"
        "CONF_GCC_LINKER_OPTS_STAGE1=-fuse-ld=gold"
        "CONF_GCC_LINKER_OPTS_STAGE2=-fuse-ld=gold"
    ] ++ lib.optionals enableDWARF [
        "--enable-dwarf-unwind"
        "--with-libdw-includes=${lib.getDev elfutils}/include"
        "--with-libdw-libraries=${lib.getLib elfutils}/lib"
    ];

  enableParallelBuilding = true;
  postPatch = "patchShebangs .";

  outputs = [ "out" "doc" "generated" ];

  # Make sure we never relax`$PATH` and hooks support for compatibility.
  strictDeps = true;

  # Don’t add -liconv to LDFLAGS automatically so that GHC will add it itself.
  dontAddExtraLibs = true;

  nativeBuildInputs = [
    perl autoconf automake m4 python3 sphinx
    ghc bootPkgs.alex bootPkgs.happy bootPkgs.hscolour
  ] ++ lib.optional (patches != []) autoreconfHook;

  # For building runtime libs
  depsBuildTarget = toolsForTarget;

  buildInputs = [ perl bash ] ++ (libDeps hostPlatform);

  depsTargetTarget = map lib.getDev (libDeps targetPlatform);
  depsTargetTargetPropagated = map (lib.getOutput "out") (libDeps targetPlatform);

  # required, because otherwise all symbols from HSffi.o are stripped, and
  # that in turn causes GHCi to abort
  stripDebugFlags = [ "-S" ] ++ lib.optional (!targetPlatform.isDarwin) "--keep-file-symbols";

  # See #63511 - the only unstripped file is the debug rts which isn't meant to
  # be stripped.
  stripDebugList = [ "lib/${name}/bin" ];

  checkTarget = "test";

  hardeningDisable = [ "format" ]
                   ++ lib.optional stdenv.targetPlatform.isAarch32 "pic"
                   ++ lib.optional stdenv.targetPlatform.isMusl "pie";

  postInstall = lib.optionalString (enableNUMA && targetPlatform.isLinux) ''
    # Patch rts.conf to ensure libnuma can be found

    for file in $(find "$out/lib" -name "rts*.conf"); do
      if grep -q numa $file; then
        substituteInPlace $file \
          --replace "library-dirs:" "library-dirs: ${targetNumactl}/lib" \
          --replace "include-dirs:" "include-dirs: ${targetNumactl}/include"
        "$out/bin/${targetPrefix}ghc-pkg" recache
      fi
    done
  '' + ''
    # Install the bash completion file.
    install -D -m 444 utils/completion/ghc.bash $out/share/bash-completion/completions/${targetPrefix}ghc

    # Patch scripts to include "readelf" and "cat" in $PATH.
    for i in "$out/bin/"*; do
      test ! -h $i || continue
      egrep --quiet '^#!' <(head -n 1 $i) || continue
      # The ghcprog fixup is for musl (where runhaskell script just needs to point to the correct
      # ghc program to work).
      sed -i \
        -e '2i export PATH="$PATH:${lib.makeBinPath [ targetPackages.stdenv.cc.bintools coreutils ]}"' \
        -e 's/ghcprog="ghc-/ghcprog="${targetPrefix}ghc-/' \
        $i
    done

    # Save generated files for needed when building ghc and ghcjs
    mkdir -p $generated/includes/dist-derivedconstants/header
    cp includes/dist-derivedconstants/header/GHCConstantsHaskell*.hs \
       $generated/includes/dist-derivedconstants/header
    if [[ -f includes/ghcplatform.h ]]; then
      cp includes/ghcplatform.h $generated/includes
    elif [[ -f includes/dist-install/build/ghcplatform.h ]]; then
      cp includes/dist-install/build/ghcplatform.h $generated/includes
    fi
    mkdir -p $generated/compiler/stage2/build
    cp compiler/stage2/build/Config.hs $generated/compiler/stage2/build || true
    if [[ -f compiler/stage2/build/GHC/Platform/Constants.hs ]]; then
      mkdir -p $generated/compiler/stage2/build/GHC/Platform
      cp compiler/stage2/build/GHC/Platform/Constants.hs $generated/compiler/stage2/build/GHC/Platform
    fi
    if [[ -f compiler/stage2/build/GHC/Settings/Config.hs ]]; then
      mkdir -p $generated/compiler/stage2/build/GHC/Settings
      cp compiler/stage2/build/GHC/Settings/Config.hs $generated/compiler/stage2/build/GHC/Settings
    fi
    cp compiler/stage2/build/*.hs-incl $generated/compiler/stage2/build || true
    mkdir -p $generated/rts/build
    cp rts/build/config.hs-incl $generated/rts/build || true

    # Save generated files for needed when building ghc-boot
    mkdir -p $generated/libraries/ghc-boot/dist-install/build/GHC/Platform
    if [[ -f libraries/ghc-boot/dist-install/build/GHC/Version.hs ]]; then
      cp libraries/ghc-boot/dist-install/build/GHC/Version.hs $generated/libraries/ghc-boot/dist-install/build/GHC/Version.hs
    fi
    if [[ -f libraries/ghc-boot/dist-install/build/GHC/Platform/Host.hs ]]; then
      cp libraries/ghc-boot/dist-install/build/GHC/Platform/Host.hs $generated/libraries/ghc-boot/dist-install/build/GHC/Platform/Host.hs
    fi

    ${installDeps targetPrefix}

    # Sanity checks for https://github.com/input-output-hk/haskell.nix/issues/660
    if ! "$out/bin/${targetPrefix}ghc" --version; then
      echo "ERROR: Missing file $out/bin/${targetPrefix}ghc"
      exit 1
    fi
    if ! "$out/bin/${targetPrefix}ghc-pkg" --version; then
      echo "ERROR: Missing file $out/bin/${targetPrefix}ghc-pkg"
      exit 1
    fi
    if [[ ! -d "$out/lib/${targetPrefix}ghc-${version}" ]]; then
      echo "ERROR: Missing directory $out/lib/${targetPrefix}ghc-${version}"
      exit 1
    fi
    if (( $(ls -1 "$out/lib/${targetPrefix}ghc-${version}" | wc -l) < 30 )); then
      echo "ERROR: Expected more files in $out/lib/${targetPrefix}ghc-${version}"
      exit 1
    fi
  '';

  passthru = {
    inherit bootPkgs targetPrefix;

    inherit llvmPackages;
    inherit enableShared;
    inherit useLLVM;

    # Our Cabal compiler name
    haskellCompilerName = "ghc-${version}";

    # This uses a similar trick to `pkgs.srcOnly` to get the configured src
    # We could add `configured-src` as an output of the ghc derivation, but
    # having it as its own derivation means it can be accessed quickly without
    # building GHC.
    configured-src = stdenv.mkDerivation ({
      name = name + "-configured-src";
      inherit
        buildInputs
        version
        nativeBuildInputs
        patches
        src
        strictDeps
        depsBuildTarget
        depsTargetTarget
        depsTargetTargetPropagated
        postPatch
        preConfigure
        configurePlatforms
        configureFlags
        outputs
        ;

      # Including all the outputs (not just $out) causes `mkDerivation` to use the nixpkgs multiple-outputs.sh hook.
      # This hook changes the arguments passed to `configure`.
      installPhase = ''
        cp -r . $out
        mkdir $doc
        mkdir $generated
      '';
      phases = [ "unpackPhase" "patchPhase" ]
            ++ lib.optional (ghc-patches != []) "autoreconfPhase"
            ++ [ "configurePhase" "installPhase"];
    });

    # Used to detect non haskell-nix compilers (accidental use of nixpkgs compilers can lead to unexpected errors)
    isHaskellNixCompiler = true;

    # The same GHC, but with debug enabled (if it can be)
    dwarf = lib.makeOverridable self (args // {
      enableDWARF = stdenv.targetPlatform.isLinux
        && builtins.compareVersions ghc-version "8.10.2" >= 0;
    });

    # The same GHC, but without the large (1TB) address space reservation
    smallAddressSpace = lib.makeOverridable self (args // {
      disableLargeAddressSpace = true;
    });
  } // extra-passthru;

  meta = {
    homepage = http://haskell.org/ghc;
    description = "The Glasgow Haskell Compiler";
    maintainers = [];
    inherit (ghc.meta) license platforms;
  };

  # Needed for `haddock` to work on source that includes non ASCII chars
  LANG = "en_US.UTF-8";
  LC_ALL = "en_US.UTF-8";
} // lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc") {
  LOCALE_ARCHIVE = "${buildPackages.glibcLocales}/lib/locale/locale-archive";
} // lib.optionalAttrs targetPlatform.useAndroidPrebuilt {
  dontStrip = true;
  dontPatchELF = true;
  noAuditTmpdir = true;
} // lib.optionalAttrs (stdenv.buildPlatform.isDarwin || stdenv.targetPlatform.isWindows) {
  # ghc install on macOS wants to run `xattr -r -c`
  # The macOS version fails because it wants python 2.
  # The nix version of xattr does not support those args.
  # Luckily setting the path to something that does not exist will skip the step.
  preBuild = lib.optionalString stdenv.buildPlatform.isDarwin ''
    export XATTR=$(mktemp -d)/nothing
  ''
  # We need to point at a stand in `windows.h` header file so that the RTS headers can
  # work on the hostPlatform.  We also need to work around case sensitve file system issues.
  + lib.optionalString stdenv.targetPlatform.isWindows ''
    export NIX_CFLAGS_COMPILE_${
        # We want this only to apply to the non windows hostPlatform (the
        # windows gcc cross compiler has a full `windows.h`).
        # This matches the way `suffixSalt` is calculated in nixpkgs.
        # See https://github.com/NixOS/nixpkgs/blob/8411006d6bcd7f6e6a8a1a80ce8fcdccdd16c6ab/pkgs/build-support/cc-wrapper/default.nix#L58
        lib.replaceStrings ["-" "."] ["_" "_"] stdenv.hostPlatform.config
      }+=" -I${../windows/include}"
    if [[ -f libraries/base/include/winio_structs.h ]]; then
      substituteInPlace libraries/base/include/winio_structs.h --replace Windows.h windows.h
    fi
    if [[ -f rts/win32/ThrIOManager.c ]]; then
      substituteInPlace rts/win32/ThrIOManager.c --replace rts\\OSThreads.h rts/OSThreads.h
    fi
  '';
});
in self
