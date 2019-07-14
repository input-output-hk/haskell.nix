{ stdenv, buildPackages, ghc, lib, pkgconfig, gobject-introspection ? null, haskellLib, makeConfigFiles, ghcForComponent, hsPkgs }:

{ componentId
, component
, package
, name
, setup
, src
, flags
, revision
, cabalFile
, cabal-generator
, patches ? []

, preUnpack ? component.preUnpack, postUnpack ? component.postUnpack
, preConfigure ? component.preConfigure, postConfigure ? component.postConfigure
, preBuild ? component.preBuild , postBuild ? component.postBuild
, preCheck ? component.preCheck , postCheck ? component.postCheck
, preInstall ? component.preInstall , postInstall ? component.postInstall
, preHaddock ? component.preHaddock , postHaddock ? component.postHaddock
, shellHook ? ""

, doCheck ? component.doCheck || haskellLib.isTest componentId
, doCrossCheck ? component.doCrossCheck || false
, dontPatchELF ? true
, dontStrip ? true

, static ? stdenv.hostPlatform.isMusl
, deadCodeElimination ? true

# Options for Haddock generation
, doHaddock ? component.doHaddock  # Enable haddock and hoogle generation
, doHoogle ? true  # Also build a hoogle index
, hyperlinkSource ? true  # Link documentation to the source code

# Profiling
, enableLibraryProfiling ? component.enableLibraryProfiling
, enableExecutableProfiling ? component.enableExecutableProfiling
, profilingDetail ? component.profilingDetail
}:

let
  cleanSrc = haskellLib.cleanCabalComponent package component src;

  fullName = if haskellLib.isAll componentId
    then "${name}-all"
    else "${name}-${componentId.ctype}-${componentId.cname}";

  configFiles = makeConfigFiles {
    inherit (package) identifier;
    inherit component fullName flags;
  };

  finalConfigureFlags = lib.concatStringsSep " " (
    [ "--prefix=$out"
      "${haskellLib.componentTarget componentId}"
      "$(cat ${configFiles}/configure-flags)"
      # GHC
      "--with-ghc=${ghc.targetPrefix}ghc"
      "--with-ghc-pkg=${ghc.targetPrefix}ghc-pkg"
      "--with-hsc2hs=${ghc.targetPrefix}hsc2hs"
    ] ++ lib.optionals (stdenv.cc != null)
    [ # CC
      "--with-gcc=${stdenv.cc.targetPrefix}cc"
      # BINTOOLS
      "--with-ld=${stdenv.cc.bintools.targetPrefix}ld"
      "--with-ar=${stdenv.cc.bintools.targetPrefix}ar"
      "--with-strip=${stdenv.cc.bintools.targetPrefix}strip"
    ] ++ [ # other flags
      (if dontStrip then "--disable-executable-stripping" else "--enable-executable-stripping")
      (if dontStrip then "--disable-library-stripping"    else "--enable-library-stripping")
      (if enableLibraryProfiling    then "--enable-library-profiling"    else "--disable-library-profiling" )
      (if enableExecutableProfiling then "--enable-executable-profiling" else "--disable-executable-profiling" )
    ] ++ lib.optional doHaddock' "--docdir=${docdir "$doc"}"
      ++ lib.optional (enableLibraryProfiling || enableExecutableProfiling) "--profiling-detail=${profilingDetail}"
      ++ lib.optional (deadCodeElimination && stdenv.hostPlatform.isLinux) "--enable-split-sections"
      ++ lib.optional (static) "--enable-static"
      ++ lib.optionals (stdenv.hostPlatform != stdenv.buildPlatform) (
        map (arg: "--hsc2hs-option=" + arg) (["--cross-compile"] ++ lib.optionals (stdenv.hostPlatform.isWindows) ["--via-asm"])
        ++ lib.optional (package.buildType == "Configure") "--configure-option=--host=${stdenv.hostPlatform.config}" )
      ++ component.configureFlags
    );

  executableToolDepends =
    (lib.concatMap (c: if c.isHaskell or false
      then builtins.attrValues (c.components.exes or {})
      else [c]) component.build-tools) ++
    lib.optional (component.pkgconfig != []) pkgconfig;

  # Unfortunately, we need to wrap ghc commands for cabal builds to
  # work in the nix-shell. See ../doc/removing-with-package-wrapper.md.
  shellWrappers = ghcForComponent {
    componentName = fullName;
    inherit configFiles;
  };

  # In order to let shell hooks make package-specific things like Hoogle databases
  shellHookApplied = if builtins.isString shellHook then shellHook else
                     if builtins.isFunction shellHook then shellHook { inherit package shellWrappers; }
                     else abort "shellHook should be a string or a function";

  # the target dir for haddock documentation
  docdir = docoutput: docoutput + "/share/doc/" + componentId.cname;

  doHaddock' = doHaddock
    && (haskellLib.isLibrary componentId)
    && stdenv.hostPlatform == stdenv.buildPlatform;

  exeExt = lib.optionalString stdenv.hostPlatform.isWindows ".exe";
  testExecutable = "dist/build/${componentId.cname}/${componentId.cname}${exeExt}";

in stdenv.lib.fix (drv:

stdenv.mkDerivation ({
  name = fullName;

  src = cleanSrc;

  inherit doCheck doCrossCheck dontPatchELF dontStrip;

  passthru = {
    inherit (package) identifier;
    config = component;
    inherit configFiles executableToolDepends;
    env = shellWrappers;

    # The directory containing the haddock documentation.
    # `null' if no haddock documentation was built.
    haddockDir = if doHaddock' then "${docdir drv.doc}/html" else null;
  };

  meta = {
    homepage = package.homepage;
    description = package.synopsis;
    license =
      let
        license-map = import ../lib/cabal-licenses.nix lib;
      in license-map.${package.license} or
        (builtins.trace "WARNING: license \"${package.license}\" not found" license-map.LicenseRef-OtherLicense);
  };

  CABAL_CONFIG = configFiles + /cabal.config;
  LANG = "en_US.UTF-8";         # GHC needs the locale configured during the Haddock phase.
  LC_ALL = "en_US.UTF-8";

  enableParallelBuilding = true;

  buildInputs = component.libs
    ++ component.frameworks
    ++ builtins.concatLists component.pkgconfig
    # Note: This is a hack until we can fix properly. See:
    # https://github.com/haskell-gi/haskell-gi/issues/226
    ++ lib.optional (lib.strings.hasPrefix "gi-" fullName) gobject-introspection;

  nativeBuildInputs =
    [shellWrappers buildPackages.removeReferencesTo]
    ++ executableToolDepends;

  SETUP_HS = setup + /bin/Setup;

  outputs = ["out" ] ++ (lib.optional doHaddock' "doc");

  # Phases
  preInstallPhases = lib.optional doHaddock' "haddockPhase";

  prePatch = if (cabalFile != null)
     then ''cat ${cabalFile} > ${package.identifier.name}.cabal''
     else lib.optionalString (cabal-generator == "hpack") ''
       ${hsPkgs.hpack.components.exes.hpack}/bin/hpack
     '';

  configurePhase = ''
    runHook preConfigure
    echo Configure flags:
    printf "%q " ${finalConfigureFlags}
    echo
    $SETUP_HS configure ${finalConfigureFlags}
    runHook postConfigure
  '';

  buildPhase = ''
    runHook preBuild
    $SETUP_HS build -j$NIX_BUILD_CORES ${lib.concatStringsSep " " component.setupBuildFlags}
    runHook postBuild
  '';

  checkPhase = ''
    runHook preCheck

    ${component.testWrapper} ${testExecutable} ${lib.concatStringsSep " " component.testFlags}

    runHook postCheck
  '';

  haddockPhase = ''
    runHook preHaddock
    docdir="${docdir "$doc"}"
    mkdir -p "$docdir"

    $SETUP_HS haddock \
      "--html" \
      ${lib.optionalString doHoogle "--hoogle"} \
      ${lib.optionalString hyperlinkSource "--hyperlink-source"} \
      ${lib.concatStringsSep " " component.setupHaddockFlags}

    html="dist/doc/html/${componentId.cname}"

    if [ -d "$html" ]; then
       # Ensure that libraries are not pulled into the docs closure.
       # As an example, the prettified source code of a
       # Paths_package module will contain store paths of the library package.
       for x in "$html/src/"*.html; do
         remove-references-to -t $out $x
       done

       cp -R "$html" "$docdir"/html
    fi
    runHook postHaddock
  '';

  # Note: Cabal does *not* copy test executables during the `install` phase.
  #
  # Note 2: if a package contains multiple libs (lib + sublibs) SETUP register will generate a
  #         folder isntead of a file for the configuration.  Therfore if the result is a folder,
  #         we need to register each and every element of that folder.
  installPhase = ''
    runHook preInstall
    $SETUP_HS copy ${lib.concatStringsSep " " component.setupInstallFlags}
    ${lib.optionalString (haskellLib.isLibrary componentId || haskellLib.isAll componentId) ''
      $SETUP_HS register --gen-pkg-config=${name}.conf
      ${ghc.targetPrefix}ghc-pkg -v0 init $out/package.conf.d
      if [ -d "${name}.conf" ]; then
        for pkg in ${name}.conf/*; do
          ${ghc.targetPrefix}ghc-pkg -v0 --package-db ${configFiles}/package.conf.d -f $out/package.conf.d register "$pkg"
        done
      else
        ${ghc.targetPrefix}ghc-pkg -v0 --package-db ${configFiles}/package.conf.d -f $out/package.conf.d register ${name}.conf
      fi
    ''}
    ${lib.optionalString (haskellLib.isTest componentId || haskellLib.isBenchmark componentId || haskellLib.isAll componentId) ''
      mkdir -p $out/${name}
      if [ -f ${testExecutable} ]; then
        cp ${testExecutable} $out/${name}/
      fi
    ''}
    runHook postInstall
  '';

  shellHook = ''
    export PATH="${shellWrappers}/bin:$PATH"
    ${shellHookApplied}
  '';
}
# patches can (if they like) depend on the version and revision of the package.
// lib.optionalAttrs (patches != []) { patches = map (p: if builtins.isFunction p then p { inherit (package.identifier) version; inherit revision; } else p) patches; }
// haskellLib.optionalHooks {
  inherit preUnpack postUnpack preConfigure postConfigure
    preBuild postBuild preCheck postCheck
    preInstall postInstall preHaddock postHaddock;
}
// lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc"){ LOCALE_ARCHIVE = "${buildPackages.glibcLocales}/lib/locale/locale-archive"; }
))
