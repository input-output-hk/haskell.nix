{ stdenv, lib, haskellLib, ghc, nonReinstallablePkgs, runCommand, writeText, writeScript }:

let
  flagsAndConfig = field: xs: lib.optionalString (xs != []) ''
    echo ${lib.concatStringsSep " " (map (x: "--${field}=${x}") xs)} >> $out/configure-flags
    echo "${field}: ${lib.concatStringsSep " " xs}" >> $out/cabal.config
  '';

  target-pkg = "${ghc.targetPrefix}ghc-pkg";

  # This is a bit of a hack.  So we'll have a slightly longer explaination here:

  # Every library component built with `comp-builder.nix` includes an `exactDep`
  # and `envDep` directory with precomputed values used here.
  # GHC derivations include `exactDep` and `envDep` directories that have
  # the same information for each of the built in packages.

  # exactDep will pass --exact-configuration to the `SETUP_HS confiugre` command.
  # This requires us to pass --dependency={dep name}={pkg id}.  The dependency
  # name will usually be the name of the package `p`, that will have been located
  # in the suitable package db when the dependency (along with `exactDep` and `envDep`)
  # was built.  Sublibs need a bit of special handling:
  #
  # - Sublibs: if the dependency is a sublibrary of a package, we need to use
  #            the sublibrary's name for the dep name, and lookup the sublibraries
  #            pkg id for z-{pkg name}-z-{sublib name}.  As we do not provide the
  #            sublib name to exactDep, as we don't have access to it at the call-site,
  #            we resort to a bit of globbing, which (as pkg db's should contain only
  #            a single package) work.

  getLibComponent = dep:
       dep.components.library # Regular package dependency
    or dep;                   # or a sublib 
  
  catPkgExactDep = p: ''
    cat ${getLibComponent p}/exactDep/configure-flags >> $out/configure-flags
    cat ${getLibComponent p}/exactDep/cabal.config >> $out/cabal.config
  '';

  catGhcPkgExactDep = p: ''
    if [ -e ${ghc}/exactDeps/${p} ]; then
      cat ${ghc}/exactDeps/${p}/configure-flags >> $out/configure-flags
      cat ${ghc}/exactDeps/${p}/cabal.config >> $out/cabal.config
    fi
  '';

  catPkgEnvDep = p: ''
    cat ${getLibComponent p}/envDep >> $out/ghc-environment
  '';

  catGhcPkgEnvDep = p: ''
    if [ -e ${ghc}/envDeps/${p} ]; then
      cat ${ghc}/envDeps/${p} >> $out/ghc-environment
    fi
  '';

in { identifier, component, fullName, flags ? {} }:
  runCommand "${fullName}-config" { nativeBuildInputs = [ghc]; } (''
    mkdir -p $out

    ${target-pkg} init $out/package.conf.d

    ${lib.concatStringsSep "\n" (lib.mapAttrsToList flagsAndConfig {
      "extra-lib-dirs" = map (p: "${lib.getLib p}/lib") component.libs;
      "extra-include-dirs" = map (p: "${lib.getDev p}/include") component.libs;
      "extra-framework-dirs" = map (p: "${p}/Library/Frameworks") component.frameworks;
    })}

    # Copy over the nonReinstallablePkgs from the global package db.
    ${lib.concatMapStringsSep "\n" (p: ''
      find ${ghc}/lib/${ghc.name}/package.conf.d -name '${p}*.conf' -exec cp -f {} $out/package.conf.d \;
    '') nonReinstallablePkgs}

    ${lib.concatMapStringsSep "\n" (p: ''
      cp -f "${(getLibComponent p).configFiles}/package.conf.d/"*.conf $out/package.conf.d
      cp -f "${getLibComponent p}/package.conf.d/"*.conf $out/package.conf.d
    '') component.depends}

    # Note: we pass `clear` first to ensure that we never consult the implicit global package db.
    ${flagsAndConfig "package-db" ["clear" "$out/package.conf.d"]}

    echo ${lib.concatStringsSep " " (lib.mapAttrsToList (fname: val: "--flags=${lib.optionalString (!val) "-" + fname}") flags)} >> $out/configure-flags

    # Provide a cabal config without remote package repositories
    echo "write-ghc-environment-files: never" >> $out/cabal.config

    # Provide a GHC environment file
    cat > $out/ghc-environment <<EOF
    package-db $out/package.conf.d
    EOF

    ${lib.concatMapStringsSep "\n" catPkgEnvDep component.depends}
    ${lib.concatMapStringsSep "\n" catGhcPkgEnvDep (lib.remove "ghc" nonReinstallablePkgs)}
  '' + lib.optionalString component.doExactConfig ''
    echo "--exact-configuration" >> $out/configure-flags
    echo "allow-newer: ${identifier.name}:*" >> $out/cabal.config
    echo "allow-older: ${identifier.name}:*" >> $out/cabal.config

    ${lib.concatMapStringsSep "\n" catPkgExactDep component.depends}
    ${lib.concatMapStringsSep "\n" catGhcPkgExactDep nonReinstallablePkgs}

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
  #
  # NOTE [ln -s -f]: we force link, as we may have dependencies that contain shared deps
  #                  (e.g. libiconv), and thus we don't want to fail, but just link it again.
  + lib.optionalString stdenv.isDarwin ''
    # Work around a limit in the macOS Sierra linker on the number of paths
    # referenced by any one dynamic library:
    #
    # Create a local directory with symlinks of the *.dylib (macOS shared
    # libraries) from all the dependencies.
    local dynamicLinksDir="$out/lib/links"
    mkdir -p $dynamicLinksDir
    for d in $(grep dynamic-library-dirs "$out/package.conf.d/"*|awk '{print $2}'|sort -u); do
      ln -f -s "$d/"*.dylib $dynamicLinksDir
    done
    # Edit the local package DB to reference the links directory.
    for f in "$out/package.conf.d/"*.conf; do
      sed -i "s,dynamic-library-dirs: .*,dynamic-library-dirs: $dynamicLinksDir," $f
    done
  '' + ''
    ${target-pkg} -v0 --package-db $out/package.conf.d recache
  '')
