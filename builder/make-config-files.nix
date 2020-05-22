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
  # name will usually be the name of the package `p`, which we can locate in the
  # package-db, passed in via `pdbArg`.  Thus querying the package-db for the
  # id field for package `p`, will unsually provide is with the right value.  Sublibs
  # need a bit of special handling:
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

  # Work our suitable packageCfgDir subdirectory
  isGhcjs        = ghc.isGhcjs or false;
  ghcCommand'    = if isGhcjs then "ghcjs" else "ghc";
  ghcCommand     = "${ghc.targetPrefix}${ghcCommand'}";
  ghcCommandCaps = lib.toUpper ghcCommand';
  libDir         = "lib/${ghcCommand}-${ghc.version}";
  packageCfgDir  = "${libDir}/package.conf.d";

in { identifier, component, fullName, flags ? {}, needsProfiling ? false }:
  # Filters out only library packages that for this GHC target
  # TODO investigate why this is needed
  # TODO find out why p ? configFiles helps (for instance for `R1909.aarch64-unknown-linux-gnu.tests.cabal-22.run.x86_64-linux`)
  let libDeps = (if needsProfiling then (x: map (p: p.profiled or p) x) else x: x)
        (lib.filter (p: (p ? configFiles) && p.configFiles.targetPrefix == ghc.targetPrefix)
         (map getLibComponent component.depends));
      cfgFiles =
        let xs = map
          (p: "${p.configFiles}")
          libDeps;
        in lib.concatStringsSep "\" \"" xs;
      libs     = lib.concatMapStringsSep "\" \"" (p: "${p}") libDeps;
  in
  runCommand "${ghc.targetPrefix}${fullName}-config" {
      nativeBuildInputs = [ghc];
      passthru = {
        inherit (ghc) targetPrefix;
        inherit ghcCommand ghcCommandCaps libDir packageCfgDir;
      };
    } (''
    mkdir -p $out

    ${target-pkg} init $out/${packageCfgDir}

    ${lib.concatStringsSep "\n" (lib.mapAttrsToList flagsAndConfig {
      "extra-lib-dirs" = map (p: "${lib.getLib p}/lib") component.libs;
      "extra-include-dirs" = map (p: "${lib.getDev p}/include") component.libs;
      "extra-framework-dirs" = map (p: "${p}/Library/Frameworks") component.frameworks;
    })}

    ghc=${ghc}
    ${ # Copy over the nonReinstallablePkgs from the global package db.
    ''
      for p in ${lib.concatStringsSep " " nonReinstallablePkgs}; do
        find $ghc/lib/${ghc.name}/package.conf.d -name $p'*.conf' -exec cp -f {} $out/${packageCfgDir} \;
      done
    ''}

    for l in "${cfgFiles}"; do
      if [ -n "$l" ]; then
        cp -f "$l/${packageCfgDir}/"*.conf $out/${packageCfgDir}
      fi
    done
    for l in "${libs}"; do
      if [ -n "$l" ]; then
        cp -f "$l/package.conf.d/"*.conf $out/${packageCfgDir}
      fi
    done

    ${ # Note: we pass `clear` first to ensure that we never consult the implicit global package db.
      flagsAndConfig "package-db" ["clear" "$out/${packageCfgDir}"]
    }

    echo ${lib.concatStringsSep " " (lib.mapAttrsToList (fname: val: "--flags=${lib.optionalString (!val) "-" + fname}") flags)} >> $out/configure-flags

    ${ # Provide a cabal config without remote package repositories
    ''
      echo "write-ghc-environment-files: never" >> $out/cabal.config
    ''}

    ${ # Provide a GHC environment file
    ''
      cat > $out/ghc-environment <<EOF
      package-db $out/${packageCfgDir}
      EOF
    ''}

    ${ lib.optionalString component.doExactConfig ''
      echo "--exact-configuration" >> $out/configure-flags
      echo "allow-newer: ${identifier.name}:*" >> $out/cabal.config
      echo "allow-older: ${identifier.name}:*" >> $out/cabal.config
    ''}

    for p in ${lib.concatStringsSep " " libDeps}; do
      cat $p/envDep >> $out/ghc-environment
      ${ lib.optionalString component.doExactConfig ''
        cat $p/exactDep/configure-flags >> $out/configure-flags
        cat $p/exactDep/cabal.config >> $out/cabal.config
      ''}
    done
    for p in ${lib.concatStringsSep " " (lib.remove "ghc" nonReinstallablePkgs)}; do
      if [ -e $ghc/envDeps/$p ]; then
        cat $ghc/envDeps/$p >> $out/ghc-environment
      fi
    done
  '' + lib.optionalString component.doExactConfig ''
    for p in ${lib.concatStringsSep " " nonReinstallablePkgs}; do
      if [ -e $ghc/exactDeps/$p ]; then
        cat $ghc/exactDeps/$p/configure-flags >> $out/configure-flags
        cat $ghc/exactDeps/$p/cabal.config >> $out/cabal.config
      fi
    done 
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
  #
  # Confusing sed stuff:
  #   '/^ ./{H;$!d} ; x'                       Groups lines that start with a space with the initial
  #                                            line of a block.  Needs a blank line added to the file
  #                                            to terminate the last block.
  #   's/ /\n/g ; s/\n\n*/\n/g; s/^\n//;'      Puts each field on its own line.
  #   's|/nix/store/|''${pkgroot}/../../../|'  Convert store path to pkgroot relative path
  #   's|''${pkgroot}/../../../|/nix/store/|'  Convert pkgroot relative path to store path

    # Work around a limit in the macOS Sierra linker on the number of paths
    # referenced by any one dynamic library:
    #
    # Create a local directory with symlinks of the *.dylib (macOS shared
    # libraries) from all the dependencies.
  + lib.optionalString stdenv.isDarwin ''
    local dynamicLinksDir="$out/lib/links"
    mkdir -p $dynamicLinksDir
    # Enumerate dynamic-library-dirs with ''${pkgroot} expanded.
    local dirsToLink=$(
      for f in "$out/${packageCfgDir}/"*.conf; do
        (cat $f; echo) | sed -En '/^ ./{H;$!d} ; x ; /^dynamic-library-dirs:/ {s/^dynamic-library-dirs:// ; s/ /\n/g ; s/\n\n*/\n/g; s/^\n//; p}'
      done | sed 's|''${pkgroot}/../../../|/nix/store/|' | sort -u
    )
    for d in $dirsToLink; do
      ln -f -s "$d/"*.{a,dylib,so} $dynamicLinksDir
    done
    # Edit the local package DB to reference the links directory.
    for f in "$out/${packageCfgDir}/"*.conf; do
      chmod +w $f
      echo >> $f
      sed -i -E "/^ ./{H;$!d} ; x ; s,^dynamic-library-dirs:.*,dynamic-library-dirs: $dynamicLinksDir," $f
    done
  '' + ''
    ${target-pkg} -v0 --package-db $out/${packageCfgDir} recache
  '')
