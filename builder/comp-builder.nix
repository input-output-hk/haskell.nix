{ stdenv, buildPackages, ghc, lib, pkgconfig, writeText, runCommand, haskellLib, nonReinstallablePkgs, withPackage, hsPkgs }:

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

, preUnpack ? null, postUnpack ? null
, preConfigure ? null, postConfigure ? null
, preBuild ? null, postBuild ? null
, preCheck ? null, postCheck ? null
, preInstall ? null, postInstall ? null
, preHaddock ? null, postHaddock ? null
, shellHook ? null

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
}:

let
  fullName = if haskellLib.isAll componentId
    then "${name}-all"
    else "${name}-${componentId.ctype}-${componentId.cname}";

  flagsAndConfig = field: xs: lib.optionalString (xs != []) ''
    echo ${lib.concatStringsSep " " (map (x: "--${field}=${x}") xs)} >> $out/configure-flags
    echo "${field}: ${lib.concatStringsSep " " xs}" >> $out/cabal.config
  '';

  flatDepends =
    let
      makePairs = map (p: rec { key="${val}"; val=(p.components.library or p); });
      closure = builtins.genericClosure {
        startSet = makePairs component.depends;
        operator = {val,...}: makePairs val.config.depends;
      };
    in map ({val,...}: val) closure;

  exactDep = pdbArg: p: ''
    if id=$(target-pkg ${pdbArg} field ${p} id --simple-output); then
      echo "--dependency=${p}=$id" >> $out/configure-flags
    fi
    if ver=$(target-pkg ${pdbArg} field ${p} version --simple-output); then
      echo "constraint: ${p} == $ver" >> $out/cabal.config
      echo "constraint: ${p} installed" >> $out/cabal.config
    fi
  '';

  envDep = pdbArg: p: ''
    if id=$(target-pkg ${pdbArg} field ${p} id --simple-output); then
      echo "package-id $id" >> $out/ghc-environment
    fi
  '';

  configFiles = runCommand "${fullName}-config" { nativeBuildInputs = [ghc]; } (''
    mkdir -p $out

    # Calls ghc-pkg for the target platform
    target-pkg() {
      ${ghc.targetPrefix}ghc-pkg "$@"
    }

    target-pkg init $out/package.conf.d

    ${lib.concatStringsSep "\n" (lib.mapAttrsToList flagsAndConfig {
      "extra-lib-dirs" = map (p: "${lib.getLib p}/lib") component.libs;
      "extra-include-dirs" = map (p: "${lib.getDev p}/include") component.libs;
      "extra-framework-dirs" = map (p: "${p}/Library/Frameworks") component.frameworks;
    })}

    # Copy over the nonReinstallablePkgs from the global package db.
    # Note: we need to use --global-package-db with ghc-pkg to prevent it
    #       from looking into the implicit global package db when registering the package.
    ${lib.concatMapStringsSep "\n" (p: ''
      target-pkg describe ${p} | target-pkg --force --global-package-db $out/package.conf.d register - || true
    '') nonReinstallablePkgs}

    ${lib.concatMapStringsSep "\n" (p: ''
      target-pkg --package-db ${p}/package.conf.d dump | target-pkg --force --package-db $out/package.conf.d register -
    '') flatDepends}

    # Note: we pass `clear` first to ensure that we never consult the implicit global package db.
    ${flagsAndConfig "package-db" ["clear" "$out/package.conf.d"]}

    echo ${lib.concatStringsSep " " (lib.mapAttrsToList (fname: val: "--flags=${lib.optionalString (!val) "-" + fname}") flags)} >> $out/configure-flags

    # Provide a GHC environment file
    cat > $out/ghc-environment <<EOF
    package-db $out/package.conf.d
    EOF
    ${lib.concatMapStringsSep "\n" (p: envDep "--package-db ${p.components.library or p}/package.conf.d" p.identifier.name) component.depends}
    ${lib.concatMapStringsSep "\n" (envDep "") (lib.remove "ghc" nonReinstallablePkgs)}

  '' + lib.optionalString component.doExactConfig ''
    echo "--exact-configuration" >> $out/configure-flags
    echo "allow-newer: ${package.identifier.name}:*" >> $out/cabal.config
    echo "allow-older: ${package.identifier.name}:*" >> $out/cabal.config

    ${lib.concatMapStringsSep "\n" (p: exactDep "--package-db ${p.components.library}/package.conf.d" p.identifier.name) component.depends}
    ${lib.concatMapStringsSep "\n" (exactDep "") nonReinstallablePkgs}

  ''
  # This code originates in the `generic-builder.nix` from nixpkgs.  However GHC has been fixed
  # to drop unused libraries referneced from libraries; and this patch is usually included in the
  # nixpkgs's GHC builds.  This doesn't sadly make this stupid hack unnecessary.  It resurfes in
  # the form of Cabal trying to be smart. Cabal when linking a library figures out that you likely
  # need those `rpath` entries, and passes `-optl-Wl,-rpath,...` for each dynamic library path to
  # GHC, thus subverting the linker and forcing it to insert all those RPATHs weather or not they
  # are needed.  We therfore reuse the linker hack here to move all al dynamic lirbaries into a
  # common folder (as links) and thus prevent Cabal from going nuts.
  #
  # TODO: Fix Cabal.
  # TODO: this is only needed if we do dynamic libraries.
  + lib.optionalString stdenv.isDarwin ''
    # Work around a limit in the macOS Sierra linker on the number of paths
    # referenced by any one dynamic library:
    #
    # Create a local directory with symlinks of the *.dylib (macOS shared
    # libraries) from all the dependencies.
    local dynamicLinksDir="$out/lib/links"
    mkdir -p $dynamicLinksDir
    for d in $(grep dynamic-library-dirs "$out/package.conf.d/"*|awk '{print $2}'|sort -u); do
      ln -s "$d/"*.dylib $dynamicLinksDir
    done
    # Edit the local package DB to reference the links directory.
    for f in "$out/package.conf.d/"*.conf; do
      sed -i "s,dynamic-library-dirs: .*,dynamic-library-dirs: $dynamicLinksDir," $f
    done
  '' + ''
    target-pkg --package-db $out/package.conf.d recache
  '' + ''
    target-pkg --package-db $out/package.conf.d check
  '');

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
      "--enable-executable-stripping"
      "--enable-library-stripping"
    ] ++ lib.optional doHaddock' "--docdir=${docdir "$doc"}"
      ++ lib.optional (deadCodeElimination && stdenv.hostPlatform.isLinux) "--enable-split-sections"
      ++ lib.optional (static) "--enable-static"
      ++ lib.optionals (stdenv.hostPlatform != stdenv.buildPlatform) (
        map (arg: "--hsc2hs-option=" + arg) (["--cross-compile"] ++ lib.optionals (stdenv.hostPlatform.isWindows) ["--via-asm"])
        ++ lib.optional (package.buildType == "Configure") "--configure-option=--host=${stdenv.hostPlatform.config}" )
      ++ component.configureFlags
  );

  executableToolDepends = lib.concatMap (c: if c.isHaskell or false
      then builtins.attrValues (c.components.exes or {})
      else [c]) component.build-tools;

  # Unfortunately, we need to wrap ghc commands for cabal builds to
  # work in the nix-shell. See ../doc/removing-with-package-wrapper.md.
  shellWrappers = withPackage {
    inherit package configFiles;
  };

  # the target dir for haddock documentation
  docdir = docoutput: docoutput + "/share/doc/" + componentId.cname;

  doHaddock' = doHaddock 
    && (haskellLib.isLibrary componentId)
    && stdenv.hostPlatform == stdenv.buildPlatform;

in stdenv.lib.fix (drv:

stdenv.mkDerivation ({
  name = fullName;

  inherit src doCheck doCrossCheck dontPatchELF dontStrip;

  passthru = {
    inherit (package) identifier;
    config = component;
    inherit configFiles;
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
  GHC_ENVIRONMENT = configFiles + /ghc-environment;
  LANG = "en_US.UTF-8";         # GHC needs the locale configured during the Haddock phase.
  LC_ALL = "en_US.UTF-8";

  enableParallelBuilding = true;

  buildInputs = component.libs
    ++ component.frameworks
    ++ component.pkgconfig;

  nativeBuildInputs =
    [ghc buildPackages.removeReferencesTo]
    ++ lib.optional (component.pkgconfig != []) pkgconfig
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
    $SETUP_HS test ${lib.concatStringsSep " " component.setupTestFlags}
    mkdir -p $out/${name}
    cp dist/test/*.log $out/${name}/
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
    ${lib.optionalString (haskellLib.isTest componentId || haskellLib.isAll componentId) ''
      mkdir -p $out/${name}
      if [ -f "dist/build/${componentId.cname}/${componentId.cname}" ]; then
        cp dist/build/${componentId.cname}/${componentId.cname} $out/${name}/
      fi
      if [ -f "dist/build/${componentId.cname}/${componentId.cname}.exe" ]; then
        cp dist/build/${componentId.cname}/${componentId.cname}.exe $out/${name}/
      fi
    ''}
    runHook postInstall
  '';

  shellHook = ''
    export PATH="${shellWrappers}/bin:$PATH"
    ${toString shellHook}
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
