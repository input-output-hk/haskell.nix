{ stdenv, lib, fetchurl
, ghc-version, ghc-version-date, ghc-patches, src-spec
, targetPrefix
, targetPlatform, hostPlatform
, targetPackages
, perl, autoconf, automake, m4, python3, sphinx, ghc, bootPkgs
, autoreconfHook, toolsForTarget, bash
, libDeps
, useLLVM, llvmPackages
, targetCC
, enableIntegerSimple, targetGmp
, enableDWARF, elfutils
, ncurses, targetLibffi, libiconv, targetIconv
, disableLargeAddressSpace
, buildMK
}:
stdenv.mkDerivation (rec {

    version = ghc-version;
    patches = ghc-patches;
    name = "${targetPrefix}ghc-${ghc-version}-configured-src";

    # Make sure we never relax`$PATH` and hooks support for compatability.
    strictDeps = true;

    nativeBuildInputs = [
        perl autoconf automake m4 python3 sphinx
        ghc bootPkgs.alex bootPkgs.happy bootPkgs.hscolour
    ] ++ lib.optional (patches != []) autoreconfHook;

    # For building runtime libs
    depsBuildTarget = toolsForTarget;

    buildInputs = [ perl bash ] ++ (libDeps hostPlatform);

    depsTargetTarget = map lib.getDev (libDeps targetPlatform);
    depsTargetTargetPropagated = map (lib.getOutput "out") (libDeps targetPlatform);

    postPatch = "patchShebangs .";

    src = if src-spec ? file
        then src-spec.file
        else fetchurl { inherit (src-spec) url sha256; };

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
        export LD="${targetCC.bintools}/bin/${targetCC.bintools.targetPrefix}ld${lib.optionalString targetPlatform.isAarch32 ".gold"}"
        export AS="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}as"
        export AR="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}ar"
        export NM="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}nm"
        export RANLIB="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}ranlib"
        export READELF="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}readelf"
        export STRIP="${targetCC.bintools.bintools}/bin/${targetCC.bintools.targetPrefix}strip"

        echo -n "${buildMK}" > mk/build.mk
        sed -i -e 's|-isysroot /Developer/SDKs/MacOSX10.5.sdk||' configure
    '' + lib.optionalString useLLVM ''
        export LLC="${llvmPackages.llvm}/bin/llc"
        export OPT="${llvmPackages.llvm}/bin/opt"
    '' + lib.optionalString (!stdenv.isDarwin) ''
        export NIX_LDFLAGS+=" -rpath $out/lib/${targetPrefix}ghc-${version}"
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
    ] ++ lib.optionals (targetPlatform.isDarwin) [
        "--without-libcharset"
    ];

    outputs = [ "out" "doc" ];
    phases = [ "unpackPhase" "patchPhase" ]
            ++ lib.optional (ghc-patches != []) "autoreconfPhase"
            ++ [ "configurePhase" "installPhase" ];
    installPhase = ''
        cp -r . $out
        mkdir $doc
    '';
})
