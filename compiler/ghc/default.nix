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
, autoconf, automake, coreutils, fetchurl, fetchpatch, perl, python3, m4, sphinx, numactl, elfutils, libcxx, libcxxabi
, autoreconfHook
, bash

, libiconv ? null

, ncurses # TODO remove this once the cross compilers all work without

, # GHC can be built with system libffi or a bundled one.
  libffi ? null

, # we don't need LLVM for x86, aarch64, or ghcjs
  useLLVM ? with stdenv.targetPlatform; !(isx86 || isAarch64 || isGhcjs)
, # LLVM is conceptually a run-time-only dependency, but for
  # non-x86, we need LLVM to bootstrap later stages, so it becomes a
  # build-time dependency too.
  buildLlvmPackages, llvmPackages

, # If enabled, GHC will be built with the GPL-free but slower integer-simple
  # library instead of the faster but GPLed integer-gmp library.
  enableIntegerSimple ? !(lib.any (lib.meta.platformMatch stdenv.hostPlatform) gmp.meta.platforms), gmp
, # If enabled, GHC will be built with the GPL-free native backend of the
  # bignum library that is nearly as fast as GMP
  enableNativeBignum ? !((lib.any (lib.meta.platformMatch stdenv.hostPlatform) gmp.meta.platforms) || enableIntegerSimple)

, # If enabled, use -fPIC when compiling static libs.
  enableRelocatedStaticLibs ? stdenv.targetPlatform != stdenv.hostPlatform && !stdenv.targetPlatform.isAarch32

, # Whether to build dynamic libs for the standard library (on the target
  # platform). Static libs are always built.
  enableShared ? !haskell-nix.haskellLib.isCrossTarget

, enableLibraryProfiling ? true

, enableDWARF ? false

, enableTerminfo ?
    # Terminfo does not work on older ghc cross arm and windows compilers
     (!haskell-nix.haskellLib.isCrossTarget || !(stdenv.targetPlatform.isAarch64 || stdenv.targetPlatform.isWindows) || builtins.compareVersions ghc-version "8.10" >= 0)

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

, useLdGold ?
    # might be better check to see if cc is clang/llvm?
    # use gold as the linker on linux to improve link times
    # do not use it on musl due to a ld.gold bug. See: <https://sourceware.org/bugzilla/show_bug.cgi?id=22266>.
    (stdenv.targetPlatform.isLinux && !stdenv.targetPlatform.isAndroid && !stdenv.targetPlatform.isMusl)

, ghc-version ? src-spec.version
, ghc-version-date ? null
, ghc-commit-id ? null
, src-spec
, ghc-patches ? []

# extra values we want to have available as passthru values.
, extra-passthru ? {}
}@args:

assert !(enableIntegerSimple || enableNativeBignum) -> gmp != null;

# Early check to make sure only one of these is enabled
assert enableNativeBignum -> !enableIntegerSimple;
assert enableIntegerSimple -> !enableNativeBignum;

let
  src = src-spec.file or (fetchurl { inherit (src-spec) url sha256; });

  inherit (stdenv) buildPlatform hostPlatform targetPlatform;
  inherit (haskell-nix.haskellLib) isCrossTarget;

  inherit (bootPkgs) ghc;

  ghcHasNativeBignum = builtins.compareVersions ghc-version "9.0" >= 0;
  hadrianHasNativeBignumFlavour = builtins.compareVersions ghc-version "9.6" >= 0;

  bignumSpec =
    assert ghcHasNativeBignum -> !enableIntegerSimple;
    assert !ghcHasNativeBignum -> !enableNativeBignum;
    if ghcHasNativeBignum then ''
      BIGNUM_BACKEND = ${if enableNativeBignum then "native" else "gmp"}
    '' else ''
      INTEGER_LIBRARY = ${if enableIntegerSimple then "integer-simple" else "integer-gmp"}
    '';

  # TODO check if this possible fix for segfaults works or not.
  targetLibffi =
    # on native platforms targetPlatform.{libffi, gmp} do not exist; thus fall back
    # to the non-targetPlatform version in those cases.
    let targetLibffi = targetPackages.libffi or libffi; in
    # we need to set `dontDisableStatic` for musl for libffi to work.
    if stdenv.targetPlatform.isMusl
    then targetLibffi.overrideAttrs (_old: { dontDisableStatic = true; })
    else targetLibffi;

  targetGmp = targetPackages.gmp or gmp;

  targetIconv = targetPackages.libiconv or libiconv;

  targetNumactl = targetPackages.numactl or numactl;

  # TODO(@Ericson2314) Make unconditional
  targetPrefix = lib.optionalString
    (targetPlatform != hostPlatform) (
      if useHadrian && targetPlatform.isGhcjs
        then "javascript-unknown-ghcjs-"
        else "${targetPlatform.config}-");

  buildMK = ''
    BuildFlavour = ${ghcFlavour}
    ifneq \"\$(BuildFlavour)\" \"\"
    include mk/flavours/\$(BuildFlavour).mk
    endif
    DYNAMIC_GHC_PROGRAMS = ${if enableShared then "YES" else "NO"}
  '' + bignumSpec + ''
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
    GhcRtsCcOpts += -fPIC
  '' + lib.optionalString (enableRelocatedStaticLibs && targetPlatform.isx86_64 && !targetPlatform.isWindows) ''
    GhcLibHcOpts += -fexternal-dynamic-refs
    GhcRtsHcOpts += -fexternal-dynamic-refs
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

  # `--with` flags for libraries needed for RTS linker
  configureFlags = [
        "--datadir=$doc/share/doc/ghc"
    ] ++ lib.optionals (!targetPlatform.isGhcjs) ["--with-curses-includes=${targetPackages.ncurses.dev}/include" "--with-curses-libraries=${targetPackages.ncurses.out}/lib"
    ] ++ lib.optionals (targetLibffi != null && !targetPlatform.isGhcjs) ["--with-system-libffi" "--with-ffi-includes=${targetLibffi.dev}/include" "--with-ffi-libraries=${targetLibffi.out}/lib"
    ] ++ lib.optionals (!enableIntegerSimple && !targetPlatform.isGhcjs) [
        "--with-gmp-includes=${targetGmp.dev}/include" "--with-gmp-libraries=${targetGmp.out}/lib"
    ] ++ lib.optionals (targetPlatform == hostPlatform && hostPlatform.libc != "glibc" && !targetPlatform.isWindows) [
        "--with-iconv-includes=${libiconv}/include" "--with-iconv-libraries=${libiconv}/lib"
    ] ++ lib.optionals (targetPlatform != hostPlatform && !targetPlatform.isGhcjs) [
        "--with-iconv-includes=${targetIconv}/include" "--with-iconv-libraries=${targetIconv}/lib"
    ] ++ lib.optionals (targetPlatform != hostPlatform) [
        "--enable-bootstrap-with-devel-snapshot"
    ] ++ lib.optionals (disableLargeAddressSpace) [
        "--disable-large-address-space"
    ] ++ lib.optionals useLdGold [
        "CFLAGS=-fuse-ld=gold"
        "CONF_GCC_LINKER_OPTS_STAGE1=-fuse-ld=gold"
        "CONF_GCC_LINKER_OPTS_STAGE2=-fuse-ld=gold"
        "CONF_LD_LINKER_OPTS_STAGE2=-fuse-ld=gold" # See: <https://gitlab.haskell.org/ghc/ghc/-/issues/22550#note_466656>
    ] ++ lib.optionals enableDWARF [
        "--enable-dwarf-unwind"
        "--with-libdw-includes=${lib.getDev elfutils}/include"
        "--with-libdw-libraries=${lib.getLib elfutils}/lib"
    ] ++ lib.optionals (targetPlatform.isDarwin && builtins.compareVersions ghc-version "9.6" >= 0) [
        # From https://github.com/NixOS/nixpkgs/commit/6454fb1bc0b5884d0c11c98a8a99735ef5a0cae8
        # Darwin uses llvm-ar. GHC will try to use `-L` with `ar` when it is `llvm-ar`
        # but it doesn’t currently work because Cabal never uses `-L` on Darwin. See:
        # https://gitlab.haskell.org/ghc/ghc/-/issues/23188
        # https://github.com/haskell/cabal/issues/8882
        "fp_cv_prog_ar_supports_dash_l=no"
    ] ++ lib.optional (targetPlatform.isGhcjs) "--target=javascript-unknown-ghcjs"; # TODO use configurePlatforms once tripple is updated in nixpkgs

  # Splicer will pull out correct variations
  libDeps = platform: lib.optional (enableTerminfo && !targetPlatform.isGhcjs) [ targetPackages.ncurses targetPackages.ncurses.dev ]
    ++ lib.optional (!targetPlatform.isGhcjs) targetLibffi
    ++ lib.optional (!enableIntegerSimple && !targetPlatform.isGhcjs) gmp
    ++ lib.optional (platform.libc != "glibc" && !targetPlatform.isWindows) libiconv
    ++ lib.optional (enableNUMA && platform.isLinux && !platform.isAarch32 && !platform.isAndroid) numactl
    # Even with terminfo disabled some older ghc cross arm and windows compilers do not build unless `ncurses` is found and they seem to want the buildPlatform version
    ++ lib.optional (!enableTerminfo && haskell-nix.haskellLib.isCrossTarget && (stdenv.targetPlatform.isAarch64 || stdenv.targetPlatform.isWindows) && builtins.compareVersions ghc-version "8.10" < 0) ncurses.dev
    ++ lib.optional enableDWARF (lib.getLib elfutils);

  toolsForTarget =
    if targetPlatform.isGhcjs
      then [ buildPackages.emscripten ]
    else if hostPlatform == buildPlatform
      then [ targetPackages.stdenv.cc ] ++ lib.optional useLLVM llvmPackages.llvm
    else assert targetPlatform == hostPlatform; # build != host == target
      [ stdenv.cc ] ++ lib.optional useLLVM buildLlvmPackages.llvm;

  targetCC = builtins.head toolsForTarget;

  useHadrian = builtins.compareVersions ghc-version "9.4" >= 0;
  # Indicates if we are installing by copying the hadrian stage1 output
  # I think we want to _always_ just install stage1. For now let's do this
  # for musl only; but I'd like to stay far away from the unnecessary
  # bindist logic as we can. It's slow, and buggy, and doesn't provide any
  # value for us.
  installStage1 = useHadrian && (haskell-nix.haskellLib.isCrossTarget || stdenv.targetPlatform.isMusl);

  hadrian =
    let
      compiler-nix-name =
        if builtins.compareVersions ghc-version "9.4.7" < 0
          then "ghc928"
          else "ghc962";
    in
      assert (buildPackages.haskell.compiler ? ${compiler-nix-name}
        || throw "Expected pkgs.haskell.compiler.${compiler-nix-name} for building hadrian");
    buildPackages.pinned-haskell-nix.tool compiler-nix-name "hadrian" {
      compilerSelection = p: p.haskell.compiler;
      index-state = buildPackages.haskell-nix.internalHackageIndexState;
      # Verions of hadrian that comes with 9.6 depends on `time`
      materialized =
        if builtins.compareVersions ghc-version "9.4" < 0
          then ../../materialized/${compiler-nix-name}/hadrian-ghc92
        else if builtins.compareVersions ghc-version "9.6" < 0
          then ../../materialized/${compiler-nix-name}/hadrian-ghc94
        else if builtins.compareVersions ghc-version "9.8" < 0
          then ../../materialized/${compiler-nix-name}/hadrian-ghc96
        else if builtins.compareVersions ghc-version "9.9" < 0
          then ../../materialized/${compiler-nix-name}/hadrian-ghc98
        else ../../materialized/${compiler-nix-name}/hadrian-ghc99;
      modules = [{
        # Apply the patches in a way that does not require using something
        # like `srcOnly`. The problem with `pkgs.srcOnly` was that it had to run
        # on a platform at eval time.
        packages.hadrian.prePatch = ''
          cd ..
        '';
        packages.hadrian.patches = ghc-patches;
        packages.hadrian.postPatch = ''
          cd hadrian
        '';
      }];
      cabalProjectLocal = null;
      cabalProjectFreeze = null;
      src = haskell-nix.haskellLib.cleanSourceWith {
        src = {
          outPath = buildPackages.srcOnly {
            name = "hadrian";
            inherit src;
          };
          filterPath = { path, ... }: path;
        };
        subDir = "hadrian";
        includeSiblings = true;
      };
    };

  # For a discription of hadrian command line args
  # see https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/README.md
  # For build flavours and flavour transformers
  # see https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/flavours.md
  hadrianArgs = "--flavour=${
        (if targetPlatform.isGhcjs then "quick" else "default")
          + lib.optionalString (!enableShared) "+no_dynamic_ghc"
          + lib.optionalString useLLVM "+llvm"
          + lib.optionalString enableDWARF "+debug_info"
          + lib.optionalString ((enableNativeBignum && hadrianHasNativeBignumFlavour) || targetPlatform.isGhcjs) "+native_bignum"
          + lib.optionalString targetPlatform.isGhcjs "+no_profiled_libs"
      } --docs=no-sphinx -j --verbose"
      # This is needed to prevent $GCC from emitting out of line atomics.
      # Those would then result in __aarch64_ldadd1_sync and others being referenced, which
      # we don't handle in the RTS properly yet. Until we figure out how to _properly_ deal
      # with the RTS_SYMBOLS in GHC, we are better off disableing the out of line atomics.
      + lib.optionalString ( hostPlatform.isAarch64 && targetPlatform.isLinux && targetPlatform.isAarch64)
        " '*.*.ghc.c.opts += -optc-mno-outline-atomics'"
      # For cross compilers only the RTS should be built with -mno-outline-atomics
      + lib.optionalString (!hostPlatform.isAarch64 && targetPlatform.isLinux && targetPlatform.isAarch64)
        " '*.rts.ghc.c.opts += -optc-mno-outline-atomics'"
      # PIC breaks GHC annotations on windows (see test/annotations for a test case)
      + lib.optionalString (enableRelocatedStaticLibs && !targetPlatform.isWindows)
        " '*.*.ghc.*.opts += -fPIC' '*.*.cc.*.opts += -fPIC'"
      # `-fexternal-dynamic-refs` causes `undefined reference` errors when building GHC cross compiler for windows
      + lib.optionalString (enableRelocatedStaticLibs && targetPlatform.isx86_64 && !targetPlatform.isWindows)
        " '*.*.ghc.*.opts += -fexternal-dynamic-refs'"
      # The following is required if we build on aarch64-darwin for aarch64-iOS. Otherwise older
      # iPhones/iPads/... won't understand the compiled code, as the compiler will emit LDSETALH
      # + lib.optionalString (targetPlatform.???) "'*.rts.ghc.c.opts += -optc-mcpu=apple-a7 -optc-march=armv8-a+norcpc'"
      # For GHC versions in the 9.x range that don't support the +native_bignum flavour transformer yet
      + lib.optionalString ((enableNativeBignum && !hadrianHasNativeBignumFlavour))
        " --bignum=native"
      ;

  # When installation is done by copying the stage1 output the directory layout
  # is different.
  rootDir =
    if installStage1
      then ""
      else "lib/${targetPrefix}ghc-${ghc-version}/";
  libDir =
    if installStage1
      then "lib"
      else "lib/${targetPrefix}ghc-${ghc-version}" + lib.optionalString (useHadrian) "/lib";
  packageConfDir = "${libDir}/package.conf.d";

  # This work around comes from nixpkgs/pkgs/development/compilers/ghc
  #
  # Sometimes we have to dispatch between the bintools wrapper and the unwrapped
  # derivation for certain tools depending on the platform.
  bintoolsFor = {
    # GHC needs install_name_tool on all darwin platforms. On aarch64-darwin it is
    # part of the bintools wrapper (due to codesigning requirements), but not on
    # x86_64-darwin.
    install_name_tool =
      if stdenv.targetPlatform.isAarch64
      then targetCC.bintools
      else targetCC.bintools.bintools;
    # Same goes for strip.
    strip =
      # TODO(@sternenseemann): also use wrapper if linker == "bfd" or "gold"
      if stdenv.targetPlatform.isAarch64
      then targetCC.bintools
      else targetCC.bintools.bintools;
  };

in
stdenv.mkDerivation (rec {
  version = ghc-version;
  name = "${targetPrefix}ghc-${version}" + lib.optionalString (useLLVM) "-llvm";

  inherit src configureFlags;
  patches = ghc-patches;

  # configure was run by configured-src already.
  phases = [ "unpackPhase" "patchPhase" "autoreconfPhase"
             "configurePhase" "buildPhase"
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
    ''
    # Use emscripten and the `config.sub` saved by `postPatch`
    + lib.optionalString (targetPlatform.isGhcjs) ''
        export CC="${targetCC}/bin/emcc"
        export CXX="${targetCC}/bin/em++"
        export LD="${targetCC}/bin/emcc"
        export NM="${targetCC}/share/emscripten/emnm"
        export EM_CACHE=$(mktemp -d)
        mv config.sub.ghcjs config.sub
    ''
    # GHC is a bit confused on its cross terminology, as these would normally be
    # the *host* tools.
    + lib.optionalString (!targetPlatform.isGhcjs) (''
        export CC="${targetCC}/bin/${targetCC.targetPrefix}cc"
        export CXX="${targetCC}/bin/${targetCC.targetPrefix}c++"
    ''
    # Use gold to work around https://sourceware.org/bugzilla/show_bug.cgi?id=16177
    + ''
        export LD="${targetCC.bintools}/bin/${targetCC.bintools.targetPrefix}ld${lib.optionalString useLdGold ".gold"}"
        export AS="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}as"
        export AR="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}ar"
        export NM="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}nm"
        export RANLIB="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}ranlib"
        export READELF="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}readelf"
        export STRIP="${bintoolsFor.strip}/bin/${bintoolsFor.strip.targetPrefix}strip"
    '' + lib.optionalString (stdenv.targetPlatform.linker == "cctools") ''
        export OTOOL="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}otool"
        export INSTALL_NAME_TOOL="${bintoolsFor.install_name_tool}/bin/${bintoolsFor.install_name_tool.targetPrefix}install_name_tool"
    '') + lib.optionalString (targetPlatform == hostPlatform && useLdGold)
    # set LD explicitly if we want gold even if we aren't cross compiling
    ''
        export LD="${targetCC.bintools}/bin/ld.gold"
    '' + lib.optionalString (targetPlatform.isWindows) ''
        export DllWrap="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}dllwrap"
        export Windres="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}windres"
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
        substituteInPlace configure.ac --replace 'RELEASE=YES' 'RELEASE=NO'
        echo '${ghc-version}' > VERSION
    '' + lib.optionalString (ghc-version-date != null) ''
        substituteInPlace configure --replace 'RELEASE=YES' 'RELEASE=NO'
        substituteInPlace configure.ac --replace 'RELEASE=YES' 'RELEASE=NO'
        echo '${ghc-version-date}' > VERSION_DATE
    '' + lib.optionalString (ghc-commit-id != null) ''
        echo '${ghc-commit-id}' > GIT_COMMIT_ID
    ''
      # The official ghc 9.2.3 tarball requires booting.
      + lib.optionalString (ghc-version == "9.2.3" || ghc-version == "9.8.20230704" || src-spec.needsBooting or false) ''
        python3 ./boot
    '';

  configurePlatforms = [ "build" "host" ] ++ lib.optional (!targetPlatform.isGhcjs) "target";

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

  depsTargetTarget = lib.optionals (!targetPlatform.isGhcjs) (map lib.getDev (libDeps targetPlatform));
  depsTargetTargetPropagated = lib.optionals (!targetPlatform.isGhcjs) (map (lib.getOutput "out") (libDeps targetPlatform));

  # required, because otherwise all symbols from HSffi.o are stripped, and
  # that in turn causes GHCi to abort
  stripDebugFlags = [ "-S" ] ++ lib.optional (!targetPlatform.isDarwin) "--keep-file-symbols";

  # See #63511 - the only unstripped file is the debug rts which isn't meant to
  # be stripped.
  stripDebugList = [ "lib/${name}/bin" ];

  checkTarget = "test";

  hardeningDisable = [ "format" ]
                   ++ lib.optional stdenv.targetPlatform.isAarch32 "pic"
                   ++ lib.optional stdenv.targetPlatform.isMusl "pie"
                   ++ lib.optional enableDWARF "fortify";

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
        -e '2i export PATH="$PATH:${lib.makeBinPath (lib.optionals (!targetPlatform.isGhcjs) [ targetPackages.stdenv.cc.bintools coreutils ])}"' \
        -e 's/ghcprog="ghc-/ghcprog="${targetPrefix}ghc-/' \
        $i
    done

    ${
      # Save generated files needed when building:
      # * The reinstallable `ghc` package (see overlays/ghc-packages.nix)
      # * The `ghcjs` package (see lib/ghcjs-project.nix).
      if useHadrian
        then
          ''
            mkdir -p $generated/includes
            if [[ -f _build/stage1/lib/ghcplatform.h ]]; then
              cp _build/stage1/lib/ghcplatform.h $generated/includes
            fi
            if [[ -f _build/stage1/compiler/build/GHC/PlatformConstants.hs ]]; then
              mkdir -p $generated/compiler/stage2/build/GHC/Platform
              cp _build/stage1/compiler/build/GHC/Platform/Constants.hs $generated/compiler/stage2/build/GHC/Platform
            fi
            mkdir -p $generated/compiler/stage2/build
            if [[ -f _build/stage1/compiler/build/GHC/Settings/Config.hs ]]; then
              mkdir -p $generated/compiler/stage2/build/GHC/Settings
              cp _build/stage1/compiler/build/GHC/Settings/Config.hs $generated/compiler/stage2/build/GHC/Settings
            fi
            cp _build/stage1/compiler/build/*.hs-incl $generated/compiler/stage2/build || true
          ''
          # Save generated files for needed when building ghc-boot
          + ''
            mkdir -p $generated/libraries/ghc-boot/dist-install/build/GHC/Platform
            if [[ -f _build/stage1/libraries/ghc-boot/build/GHC/Version.hss ]]; then
              cp _build/stage1/libraries/ghc-boot/build/GHC/Version.hs $generated/libraries/ghc-boot/dist-install/build/GHC/Version.hs
            fi
            if [[ -f _build/stage1/libraries/ghc-boot/build/GHC/Platform/Host.hs ]]; then
              cp _build/stage1/libraries/ghc-boot/build/GHC/Platform/Host.hs $generated/libraries/ghc-boot/dist-install/build/GHC/Platform/Host.hs
            fi
          ''
          # Convert ${pkgroot} relative paths to /nix/store paths
          + ''
            for f in "$out/${packageConfDir}/"*.conf; do
              sed -i -e "s|\''${pkgroot}/../lib/|$out/${rootDir}lib/|" \
                     -e "s|\''${pkgroot}/../share/|$out/${rootDir}share/|" \
                     -e "s|\''${pkgroot}/../../../share/doc/|$doc/share/doc/|" $f
            done
          ''
        else
          ''
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
        ''
    }

    # Sanity checks for https://github.com/input-output-hk/haskell.nix/issues/660
    if ! "$out/bin/${targetPrefix}ghc" --version; then
      echo "ERROR: Missing file $out/bin/${targetPrefix}ghc"
      exit 1
    fi
    if ! "$out/bin/${targetPrefix}ghc-pkg" --version; then
      echo "ERROR: Missing file $out/bin/${targetPrefix}ghc-pkg"
      exit 1
    fi
    if [[ ! -d "$out/${packageConfDir}" ]]; then
      echo "ERROR: Missing directory $out/${packageConfDir}"
      exit 1
    fi
    if (( $(ls -1 "$out/${packageConfDir}" | wc -l) < 30 )); then
      echo "ERROR: Expected more files in $out/${packageConfDir}"
      exit 1
    fi
    '';

  passthru = {
    inherit bootPkgs targetPrefix libDir llvmPackages enableShared useLLVM;

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
      phases = [ "unpackPhase" "patchPhase" "autoreconfPhase"
                 "configurePhase" "installPhase"];
     } // lib.optionalAttrs useHadrian {
      postConfigure = ''
        for a in libraries/*/*.cabal.in utils/*/*.cabal.in compiler/ghc.cabal.in; do
          ${hadrian}/bin/hadrian ${hadrianArgs} "''${a%.*}"
        done
      '' + lib.optionalString (ghc-version == "9.8.20230704") ''
        for a in bytearray-access-ops.txt.pp addr-access-ops.txt.pp primops.txt; do
          ${hadrian}/bin/hadrian ${hadrianArgs} _build/stage0/compiler/build/$a
          cp _build/stage0/compiler/build/$a compiler/GHC/Builtin/$a
        done
      '' + lib.optionalString stdenv.isDarwin ''
        substituteInPlace mk/system-cxx-std-lib-1.0.conf \
          --replace 'dynamic-library-dirs:' 'dynamic-library-dirs: ${libcxx}/lib ${libcxxabi}/lib'
        find . -name 'system*.conf*'
        cat mk/system-cxx-std-lib-1.0.conf
      '' + lib.optionalString (installStage1 && stdenv.targetPlatform.isMusl) ''
        substituteInPlace hadrian/cfg/system.config \
          --replace 'cross-compiling       = YES' \
                    'cross-compiling       = NO'
      '';
    } // lib.optionalAttrs targetPlatform.isGhcjs {
      # Backup the config.sub that knows what `ghcjs` is in case
      # `autoreconfPhase` replaces it
      postPatch = ''
        cp config.sub config.sub.ghcjs
      '';
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
} // lib.optionalAttrs targetPlatform.isGhcjs {
  # Backup the config.sub that knows what `ghcjs` is in case
  # `autoreconfPhase` replaces it
  postPatch = ''
    cp config.sub config.sub.ghcjs
  '';
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
  # Same hack as 'preBuild'
  preInstall = lib.optionalString stdenv.buildPlatform.isDarwin ''
    export XATTR=$(mktemp -d)/nothing
  '';
} // lib.optionalAttrs useHadrian {
  postConfigure = lib.optionalString stdenv.isDarwin ''
    substituteInPlace mk/system-cxx-std-lib-1.0.conf \
      --replace 'dynamic-library-dirs:' 'dynamic-library-dirs: ${libcxx}/lib ${libcxxabi}/lib'
    find . -name 'system*.conf*'
    cat mk/system-cxx-std-lib-1.0.conf
  '' + lib.optionalString (installStage1 && !haskell-nix.haskellLib.isCrossTarget && stdenv.targetPlatform.isMusl) ''
    substituteInPlace hadrian/cfg/system.config \
      --replace 'cross-compiling       = YES' \
                'cross-compiling       = NO'
  '';
  buildPhase = ''
    ${hadrian}/bin/hadrian ${hadrianArgs}
  '' + lib.optionalString (installStage1 && !stdenv.targetPlatform.isGhcjs && builtins.compareVersions ghc-version "9.8" < 0) ''
    ${hadrian}/bin/hadrian ${hadrianArgs} stage1:lib:libiserv
  '' + lib.optionalString targetPlatform.isMusl ''
    ${hadrian}/bin/hadrian ${hadrianArgs} stage1:lib:terminfo
  '' + lib.optionalString (installStage1 && !haskell-nix.haskellLib.isCrossTarget) ''
    ${hadrian}/bin/hadrian ${hadrianArgs} stage2:exe:iserv
    # I don't seem to be able to build _build/stage1/lib/bin/ghc-iserv-prof
    # by asking hadrian for this. The issue is likely that the profiling way
    # is probably missing from hadrian m(
    ${hadrian}/bin/hadrian ${hadrianArgs} _build/stage1/lib/bin/ghc-iserv-prof
    pushd _build/stage1/bin
    for exe in *; do
      mv $exe ${targetPrefix}$exe
    done
    popd
  '';

  # Hadrian's installation only works for native compilers, and is broken for cross compilers.
  # However Hadrian produces mostly relocatable installs anyway, so we can simply copy
  # stage1/{bin, lib, share} into the destination as the copy phase.

  installPhase =
    if installStage1
      then ''
        mkdir $out
        cp -r _build/stage1/bin $out
        ${
          # These are needed when building the reinstallable lib ghc
          if targetPlatform.isMusl
            then ''
              cp _build/stageBoot/bin/genprimopcode $out/bin
              cp _build/stageBoot/bin/deriveConstants $out/bin
            '' else ''
              cp _build/stageBoot/bin/${targetPrefix}genprimopcode $out/bin
              ln -s $out/bin/${targetPrefix}genprimopcode $out/bin/genprimopcode
              cp _build/stageBoot/bin/${targetPrefix}deriveConstants $out/bin
              ln -s $out/bin/${targetPrefix}deriveConstants $out/bin/deriveConstants
            ''
        }
        cp -r _build/stage1/lib $out
        mkdir $doc
        cp -r _build/stage1/share $doc
        runHook postInstall
      ''
      # there appears to be a bug in GHCs configure script not properly passing dllwrap, and windres to the
      # generated settings file. Hence we patch it back in here.
      + lib.optionalString (targetPlatform.isWindows) ''
        substituteInPlace $out/lib/settings \
          --replace ',("dllwrap command", "/bin/false")' ',("dllwrap command", "${targetCC.bintools.targetPrefix}dllwrap")' \
          --replace ',("windres command", "/bin/false")' ',("windres command", "${targetCC.bintools.targetPrefix}windres")'
      ''
      else ''
        runHook preInstall
        ${hadrian}/bin/hadrian ${hadrianArgs} binary-dist-dir
        cd _build/bindist/ghc-*
        ./configure --prefix=$out ${lib.concatStringsSep " " configureFlags}
        ${lib.optionalString stdenv.isDarwin ''
          substituteInPlace mk/system-cxx-std-lib-1.0.conf \
            --replace 'dynamic-library-dirs:' 'dynamic-library-dirs: ${libcxx}/lib ${libcxxabi}/lib'
          substituteInPlace lib/package.conf.d/system-cxx-std-lib-1.0.conf \
            --replace 'dynamic-library-dirs:' 'dynamic-library-dirs: ${libcxx}/lib ${libcxxabi}/lib'
        ''}
        mkdir -p utils
        cp -r ../../../utils/completion utils
        make install
        cd ../../..
        runHook postInstall
      '';
});
in self
