# This is the nix expression to build ghc. The Glasgow Haskell Compiler. It is
# heavily inspired (and based) upon the ghc expression from nixos/nixpkgs.
# haskell.nix ships its own version of the ghc expression as it needs more
# control over the expression to isolate it against varying <nixpkgs> and
# allow us to customize it to the way haskell.nix works.
{ stdenv, haskell-nix, targetPackages

# build-tools
, bootPkgs
, autoconf, automake, coreutils, fetchurl, fetchpatch, perl, python3, m4, sphinx, numactl
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
  enableIntegerSimple ? !(stdenv.lib.any (stdenv.lib.meta.platformMatch stdenv.hostPlatform) gmp.meta.platforms), gmp

, # If enabled, use -fPIC when compiling static libs.
  enableRelocatedStaticLibs ? stdenv.targetPlatform != stdenv.hostPlatform && !stdenv.targetPlatform.isAarch32

, # Whether to build dynamic libs for the standard library (on the target
  # platform). Static libs are always built.
  enableShared ? !haskell-nix.haskellLib.isCrossTarget

, enableLibraryProfiling ? true

, # Whether to build terminfo.  Musl fails to build terminfo as ncurses seems to be linked to glibc
  enableTerminfo ? !stdenv.targetPlatform.isWindows && !stdenv.targetPlatform.isMusl

, # What flavour to build. An empty string indicates no
  # specific flavour and falls back to ghc default values.
  ghcFlavour ? stdenv.lib.optionalString haskell-nix.haskellLib.isCrossTarget (
    if useLLVM
      then "perf-cross"
      else "perf-cross-ncg"
    )

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
    CrossCompilePrefix = ${targetPrefix}
  '' + stdenv.lib.optionalString isCrossTarget ''
    Stage1Only = ${if targetPlatform.system == hostPlatform.system then "NO" else "YES"}
    HADDOCK_DOCS = NO
    BUILD_SPHINX_HTML = NO
    BUILD_SPHINX_PDF = NO
  '' + stdenv.lib.optionalString enableRelocatedStaticLibs ''
    GhcLibHcOpts += -fPIC
    GhcRtsHcOpts += -fPIC
  '' + stdenv.lib.optionalString targetPlatform.useAndroidPrebuilt ''
    EXTRA_CC_OPTS += -std=gnu99
  '' + stdenv.lib.optionalString (!enableTerminfo) ''
    WITH_TERMINFO=NO
  ''
  # musl doesn't have a system-linker. Only on x86, and on x86 we need it, as
  # our elf linker for x86_64 is broken.
  + stdenv.lib.optionalString (targetPlatform.isMusl && !targetPlatform.isx86) ''
    compiler_CONFIGURE_OPTS += --flags=-dynamic-system-linker
  ''
  # While split sections are now enabled by default in ghc 8.8 for windows,
  # the seem to lead to `too many sections` errors when building base for
  # profiling.
  + stdenv.lib.optionalString targetPlatform.isWindows ''
    SplitSections = NO
  '' + stdenv.lib.optionalString (!enableLibraryProfiling) ''
    BUILD_PROF_LIBS = NO
  '';

  # Splicer will pull out correct variations
  libDeps = platform: stdenv.lib.optional enableTerminfo [ ncurses ]
    ++ [targetLibffi]
    ++ stdenv.lib.optional (!enableIntegerSimple) gmp
    ++ stdenv.lib.optional (platform.libc != "glibc" && !targetPlatform.isWindows) libiconv
    ++ stdenv.lib.optional (platform.isLinux && !platform.isAarch32) numactl;

  toolsForTarget =
    if hostPlatform == buildPlatform then
      [ targetPackages.stdenv.cc ] ++ stdenv.lib.optional useLLVM llvmPackages.llvm
    else assert targetPlatform == hostPlatform; # build != host == target
      [ stdenv.cc ] ++ stdenv.lib.optional useLLVM buildLlvmPackages.llvm;

  targetCC = builtins.head toolsForTarget;

  configured-src = import ./configured-src.nix {
    inherit stdenv fetchurl
    ghc-version ghc-patches src-spec
    targetPrefix
    targetPlatform hostPlatform
    targetPackages
    perl autoconf automake m4 python3 sphinx ghc bootPkgs
    autoreconfHook toolsForTarget bash
    libDeps
    useLLVM llvmPackages
    targetCC
    enableIntegerSimple targetGmp
    ncurses targetLibffi libiconv
    disableLargeAddressSpace
    buildMK
    ;
  };
in
stdenv.mkDerivation (rec {
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

  # ghc hardcodes the TOP dir during config, this breaks when
  # splitting the configured src from the build process.
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

  outputs = [ "out" "doc" "generated" ];

  # Make sure we never relax`$PATH` and hooks support for compatibility.
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
      # The ghcprog fixup is for musl (where runhaskell script just needs to point to the correct
      # ghc program to work).
      sed -i \
        -e '2i export PATH="$PATH:${stdenv.lib.makeBinPath [ targetPackages.stdenv.cc.bintools coreutils ]}"' \
        -e 's/ghcprog="ghc-/ghcprog="${targetPrefix}ghc-/' \
        $i
    done

    # Save generated files for needed when building ghcjs
    mkdir -p $generated/includes/dist-derivedconstants/header
    cp includes/dist-derivedconstants/header/GHCConstantsHaskellExports.hs \
       includes/dist-derivedconstants/header/GHCConstantsHaskellType.hs \
       includes/dist-derivedconstants/header/GHCConstantsHaskellWrappers.hs \
       $generated/includes/dist-derivedconstants/header
    if [[ -f includes/ghcplatform.h ]]; then
      cp includes/ghcplatform.h $generated/includes
    elif [[ -f includes/dist-install/build/ghcplatform.h ]]; then
      cp includes/dist-install/build/ghcplatform.h $generated/includes
    fi
    mkdir -p $generated/compiler/stage2/build
    cp compiler/stage2/build/Config.hs $generated/compiler/stage2/build || true
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
    if [[ ! -f "$out/bin/${targetPrefix}ghc" ]]; then
      echo "ERROR: Missing file $out/bin/${targetPrefix}ghc"
      exit 0
    fi
    if [[ ! -f "$out/bin/${targetPrefix}ghc-pkg" ]]; then
      echo "ERROR: Missing file $out/bin/${targetPrefix}ghc-pkg"
      exit 0
    fi
    if [[ ! -d "$out/lib/${targetPrefix}ghc-${version}" ]]; then
      echo "ERROR: Missing directory $out/lib/${targetPrefix}ghc-${version}"
      exit 0
    fi
    if (( $(ls -1 "$out/lib/${targetPrefix}ghc-${version}" | wc -l) < 30 )); then
      echo "ERROR: Expected more files in $out/lib/${targetPrefix}ghc-${version}"
      exit 0
    fi
  '';

  passthru = {
    inherit bootPkgs targetPrefix;

    inherit llvmPackages;
    inherit enableShared;

    # Our Cabal compiler name
    haskellCompilerName = "ghc-${version}";

    configured-src = configured-src;

    # Used to detect non haskell-nix compilers (accidental use of nixpkgs compilers can lead to unexpected errors)
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
})
