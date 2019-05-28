{ stdenv, lib, haskellLib, ghc, nonReinstallablePkgs, runCommand }:

let
  flagsAndConfig = field: xs: lib.optionalString (xs != []) ''
    echo ${lib.concatStringsSep " " (map (x: "--${field}=${x}") xs)} >> $out/configure-flags
    echo "${field}: ${lib.concatStringsSep " " xs}" >> $out/cabal.config
  '';

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

in { identifier, component, fullName, flags ? {} }:

  runCommand "${fullName}-config" { nativeBuildInputs = [ghc]; } (''
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
    '') (haskellLib.flatLibDepends component)}

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
    echo "allow-newer: ${identifier.name}:*" >> $out/cabal.config
    echo "allow-older: ${identifier.name}:*" >> $out/cabal.config

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
  '')
