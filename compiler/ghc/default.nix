# This is the nix expression to build ghc. The Glasgow Haskell Compiler. It is
# heavily inspired (and based) upon the ghc expression from nixos/nixpkgs.
# haskell.nix ships its own version of the ghc expression as it needs more
# control over the expression to isolate it against varying <nixpkgs> and
# allow us to customize it to the way haskell.nix works.
{ stdenv, targetPackages

# build-tools
, bootPkgs
, autoconf, automake, coreutils, fetchurl, fetchpatch, perl, python3, m4, sphinx
, autoreconfHook
, bash

, libiconv ? null, ncurses

, installDeps

, # GHC can be built with system libffi or a bundled one.
  libffi ? null

, useLLVM ? !stdenv.targetPlatform.isx86
, # LLVM is conceptually a run-time-only depedendency, but for
  # non-x86, we need LLVM to bootstrap later stages, so it becomes a
  # build-time dependency too.
  buildLlvmPackages, llvmPackages

, # If enabled, GHC will be built with the GPL-free but slower integer-simple
  # library instead of the faster but GPLed integer-gmp library.
  enableIntegerSimple ? !(stdenv.lib.any (stdenv.lib.meta.platformMatch stdenv.hostPlatform) gmp.meta.platforms), gmp

, # If enabled, use -fPIC when compiling static libs.
  enableRelocatedStaticLibs ? stdenv.targetPlatform != stdenv.hostPlatform && !stdenv.targetPlatform.isAarch32

, # Whether to build dynamic libs for the standard library (on the target
  # platform). Static libs are always built.
  enableShared ? !stdenv.targetPlatform.isWindows && !stdenv.targetPlatform.useiOSPrebuilt

, # Whetherto build terminfo.
  enableTerminfo ? !stdenv.targetPlatform.isWindows

, # What flavour to build. An empty string indicates no
  # specific flavour and falls back to ghc default values.
  ghcFlavour ? stdenv.lib.optionalString (stdenv.targetPlatform != stdenv.hostPlatform)
    (if useLLVM then "perf-cross" else "perf-cross-ncg")

, # Whether to disable the large address space allocator
  # necessary fix for iOS: https://www.reddit.com/r/haskell/comments/4ttdz1/building_an_osxi386_to_iosarm64_cross_compiler/d5qvd67/
  disableLargeAddressSpace ? stdenv.targetPlatform.isDarwin && stdenv.targetPlatform.isAarch64

, ghc-version ? src-spec.version
, src-spec
, ghc-patches ? []

# extra values we want to have available as passthru values.
, extra-passthru ? {}
}:

assert !enableIntegerSimple -> gmp != null;

let
  inherit (stdenv) buildPlatform hostPlatform targetPlatform;

  inherit (bootPkgs) ghc;

  # TODO(@Ericson2314) Make unconditional
  targetPrefix = stdenv.lib.optionalString
    (targetPlatform != hostPlatform)
    "${targetPlatform.config}-";

  buildMK = ''
    BuildFlavour = ${ghcFlavour}
    ifneq \"\$(BuildFlavour)\" \"\"
    include mk/flavours/\$(BuildFlavour).mk
    endif
    DYNAMIC_GHC_PROGRAMS = ${if enableShared then "YES" else "NO"}
    INTEGER_LIBRARY = ${if enableIntegerSimple then "integer-simple" else "integer-gmp"}
  '' + stdenv.lib.optionalString (targetPlatform != hostPlatform) ''
    Stage1Only = ${if targetPlatform.system == hostPlatform.system then "NO" else "YES"}
    CrossCompilePrefix = ${targetPrefix}
    HADDOCK_DOCS = NO
    BUILD_SPHINX_HTML = NO
    BUILD_SPHINX_PDF = NO
  '' + stdenv.lib.optionalString enableRelocatedStaticLibs ''
    GhcLibHcOpts += -fPIC
    GhcRtsHcOpts += -fPIC
  '' + stdenv.lib.optionalString targetPlatform.useAndroidPrebuilt ''
    EXTRA_CC_OPTS += -std=gnu99
  '' + stdenv.lib.optionalString useLLVM ''
    GhcStage2HcOpts += -fast-llvm
    GhcLibHcOpts += -fast-llvm
  '';

  # Splicer will pull out correct variations
  libDeps = platform: stdenv.lib.optional enableTerminfo [ ncurses ]
    ++ [libffi]
    ++ stdenv.lib.optional (!enableIntegerSimple) gmp
    ++ stdenv.lib.optional (platform.libc != "glibc" && !targetPlatform.isWindows) libiconv;

  toolsForTarget =
    if hostPlatform == buildPlatform then
      [ targetPackages.stdenv.cc ] ++ stdenv.lib.optional useLLVM llvmPackages.llvm
    else assert targetPlatform == hostPlatform; # build != host == target
      [ stdenv.cc ] ++ stdenv.lib.optional useLLVM buildLlvmPackages.llvm;

  targetCC = builtins.head toolsForTarget;

in let configured-src = stdenv.mkDerivation (rec {

        version = ghc-version;

        patches = ghc-patches;

        name = "${targetPrefix}ghc-${ghc-version}-configured-src";


  nativeBuildInputs = [
    perl autoconf automake m4 python3 sphinx
    ghc bootPkgs.alex bootPkgs.happy bootPkgs.hscolour
  ] ++ stdenv.lib.optional (patches != []) autoreconfHook;

  # For building runtime libs
  depsBuildTarget = toolsForTarget;

  buildInputs = [ perl bash ] ++ (libDeps hostPlatform);

  propagatedBuildInputs = [ targetPackages.stdenv.cc ]
    ++ stdenv.lib.optional useLLVM llvmPackages.llvm;

  depsTargetTarget = map stdenv.lib.getDev (libDeps targetPlatform);
  depsTargetTargetPropagated = map (stdenv.lib.getOutput "out") (libDeps targetPlatform);

  postPatch = "patchShebangs .";

        src = fetchurl { inherit (src-spec) url sha256; };

        # GHC is a bit confused on its cross terminology.
        preConfigure = ''
            for env in $(env | grep '^TARGET_' | sed -E 's|\+?=.*||'); do
            export "''${env#TARGET_}=''${!env}"
            done
            # GHC is a bit confused on its cross terminology, as these would normally be
            # the *host* tools.
            export CC="${targetCC}/bin/${targetCC.targetPrefix}cc"
            export CXX="${targetCC}/bin/${targetCC.targetPrefix}cxx"
            # Use gold to work around https://sourceware.org/bugzilla/show_bug.cgi?id=16177
            export LD="${targetCC.bintools}/bin/${targetCC.bintools.targetPrefix}ld${stdenv.lib.optionalString targetPlatform.isAarch32 ".gold"}"
            export AS="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}as"
            export AR="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}ar"
            export NM="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}nm"
            export RANLIB="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}ranlib"
            export READELF="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}readelf"
            export STRIP="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}strip"

            echo -n "${buildMK}" > mk/build.mk
            sed -i -e 's|-isysroot /Developer/SDKs/MacOSX10.5.sdk||' configure
        '' + stdenv.lib.optionalString (!stdenv.isDarwin) ''
            export NIX_LDFLAGS+=" -rpath $out/lib/ghc-${version}"
        '' + stdenv.lib.optionalString stdenv.isDarwin ''
            export NIX_LDFLAGS+=" -no_dtrace_dof"
        '' + stdenv.lib.optionalString targetPlatform.useAndroidPrebuilt ''
            sed -i -e '5i ,("armv7a-unknown-linux-androideabi", ("e-m:e-p:32:32-i64:64-v128:64:128-a:0:32-n32-S64", "cortex-a8", ""))' llvm-targets
        '' + stdenv.lib.optionalString targetPlatform.isMusl ''
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
        '';

        # TODO(@Ericson2314): Always pass "--target" and always prefix.
        configurePlatforms = [ "build" "host" ]
            ++ stdenv.lib.optional (targetPlatform != hostPlatform) "target";
        # `--with` flags for libraries needed for RTS linker
        configureFlags = [
            "--datadir=$doc/share/doc/ghc"
            "--with-curses-includes=${ncurses.dev}/include" "--with-curses-libraries=${ncurses.out}/lib"
        ] ++ stdenv.lib.optionals (libffi != null) ["--with-system-libffi" "--with-ffi-includes=${targetPackages.libffi.dev}/include" "--with-ffi-libraries=${targetPackages.libffi.out}/lib"
        ] ++ stdenv.lib.optional (!enableIntegerSimple) [
            "--with-gmp-includes=${targetPackages.gmp.dev}/include" "--with-gmp-libraries=${targetPackages.gmp.out}/lib"
        ] ++ stdenv.lib.optional (targetPlatform == hostPlatform && hostPlatform.libc != "glibc" && !targetPlatform.isWindows) [
            "--with-iconv-includes=${libiconv}/include" "--with-iconv-libraries=${libiconv}/lib"
        ] ++ stdenv.lib.optionals (targetPlatform != hostPlatform) [
            "--enable-bootstrap-with-devel-snapshot"
        ] ++ stdenv.lib.optionals (targetPlatform.isAarch32) [
            "CFLAGS=-fuse-ld=gold"
            "CONF_GCC_LINKER_OPTS_STAGE1=-fuse-ld=gold"
            "CONF_GCC_LINKER_OPTS_STAGE2=-fuse-ld=gold"
        ] ++ stdenv.lib.optionals (disableLargeAddressSpace) [
            "--disable-large-address-space"
        ];

        outputs = [ "out" ];
        phases = [ "unpackPhase" "patchPhase" ]
              ++ stdenv.lib.optional (ghc-patches != []) "autoreconfPhase"
              ++ [ "configurePhase" "installPhase" ];
        installPhase = "cp -r . $out";
    });

  drv =stdenv.mkDerivation (rec {
  version = ghc-version;
  name = "${targetPrefix}ghc-${version}";

  patches = ghc-patches;

  # for this to properly work (with inheritance of patches, postPatch, ...)
  # this needs to be a function over the values we want to inherit and then called
  # accordingly. Most trivial might be to just have args, and mash them into the
  # attrset.
  src = configured-src;

  # configure was run by configured-src already.
  phases = [ "unpackPhase" "buildPhase"
             "checkPhase" "installPhase"
             "fixupPhase"
             "installCheckPhase"
             "distPhase"
             ];

  # ghc hardcodes the TOP dir durcing config, this breaks when
  # splitting the configured src from the the build process.
  postUnpack = ''
    (cd $sourceRoot
     TOP=$(cat mk/config.mk|grep ^TOP|awk -F\  '{ print $3 }')
     PREFIX=$(cat mk/install.mk|grep ^prefix|awk -F\  '{ print $3 }')

     # these two are required
     substituteInPlace mk/config.mk  --replace "$TOP" "$PWD" \
                                     --replace "$PREFIX" "$out"

     substituteInPlace mk/install.mk --replace "$TOP" "$PWD" \
                                     --replace "$PREFIX" "$out"

     # these two only for convencience.
     substituteInPlace config.log    --replace "$TOP" "$PWD"\
                                     --replace "$PREFIX" "$out"
     substituteInPlace config.status --replace "$TOP" "$PWD"\
                                     --replace "$PREFIX" "$out")


  '';

  enableParallelBuilding = true;
  postPatch = "patchShebangs .";

  outputs = [ "out" "doc" ];

  # Make sure we never relax`$PATH` and hooks support for compatability.
  strictDeps = true;

  # Don’t add -liconv to LDFLAGS automatically so that GHC will add it itself.
  dontAddExtraLibs = true;

  nativeBuildInputs = [
    perl autoconf automake m4 python3 sphinx
    ghc bootPkgs.alex bootPkgs.happy bootPkgs.hscolour
  ] ++ stdenv.lib.optional (patches != []) autoreconfHook;

  # For building runtime libs
  depsBuildTarget = toolsForTarget;

  buildInputs = [ perl bash ] ++ (libDeps hostPlatform);

  propagatedBuildInputs = [ targetPackages.stdenv.cc ]
    ++ stdenv.lib.optional useLLVM llvmPackages.llvm;

  depsTargetTarget = map stdenv.lib.getDev (libDeps targetPlatform);
  depsTargetTargetPropagated = map (stdenv.lib.getOutput "out") (libDeps targetPlatform);

  # required, because otherwise all symbols from HSffi.o are stripped, and
  # that in turn causes GHCi to abort
  stripDebugFlags = [ "-S" ] ++ stdenv.lib.optional (!targetPlatform.isDarwin) "--keep-file-symbols";

  # See #63511 - the only unstripped file is the debug rts which isn't meant to
  # be stripped.
  stripDebugList = [ "lib/${name}/bin" ];

  checkTarget = "test";

  hardeningDisable = [ "format" ]
                   ++ stdenv.lib.optional stdenv.targetPlatform.isAarch32 "pic"
                   ++ stdenv.lib.optional stdenv.targetPlatform.isMusl "pie";

  postInstall = ''
    # Install the bash completion file.
    install -D -m 444 utils/completion/ghc.bash $out/share/bash-completion/completions/${targetPrefix}ghc

    # Patch scripts to include "readelf" and "cat" in $PATH.
    for i in "$out/bin/"*; do
      test ! -h $i || continue
      egrep --quiet '^#!' <(head -n 1 $i) || continue
      sed -i -e '2i export PATH="$PATH:${stdenv.lib.makeBinPath [ targetPackages.stdenv.cc.bintools coreutils ]}"' $i
    done
  '' + installDeps targetPrefix;

  passthru = {
    inherit bootPkgs targetPrefix;

    inherit llvmPackages;
    inherit enableShared;

    # Our Cabal compiler name
    haskellCompilerName = "ghc-${version}";

    configured-src = configured-src;

    # Used to detect non haskell-nix compilers (accedental use of nixpkgs compilers can lead to unexpected errors)
    isHaskellNixCompiler = true;
  } // extra-passthru;

  meta = {
    homepage = http://haskell.org/ghc;
    description = "The Glasgow Haskell Compiler";
    maintainers = [];
    inherit (ghc.meta) license platforms;
  };

} // stdenv.lib.optionalAttrs targetPlatform.useAndroidPrebuilt {
  dontStrip = true;
  dontPatchELF = true;
  noAuditTmpdir = true;
});

in drv