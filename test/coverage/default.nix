{ stdenv, lib, cabal-install, cabalProject', stackProject', recurseIntoAttrs, runCommand, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  projectArgs = {
    inherit evalPackages;
    src = testSrc "coverage";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = [{
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.bytestring-builder.doHaddock = false;

      # Coverage
      packages.pkga.components.library.doCoverage = true;
      packages.pkgb.components.library.doCoverage = true;
    }];
  };

  # We can easily select a different compiler when using cabal,
  # but for stack we would need a different resolver to be used..
  cabalProj = (cabalProject' (projectArgs // { inherit compiler-nix-name; }));
  stackProj = (stackProject' projectArgs);

  exeExt = stdenv.hostPlatform.extensions.executable;
  crossSuffix = lib.optionalString (stdenv.hostPlatform != stdenv.buildPlatform) "-${stdenv.hostPlatform.config}";

in recurseIntoAttrs ({
  # Does not work on ghcjs because it needs zlib.
  meta.disabled = stdenv.hostPlatform.isGhcjs;
  run = stdenv.mkDerivation {
    name = "coverage-test";

    buildCommand = ''
      ########################################################################
      # test coverage reports with an example project

      fileExistsNonEmpty() {
        local file=$1
        if [ ! -f "$file" ]; then
          echo "Missing: $file"
          exit 1
        fi
        local filesize=$(command stat --format '%s' "$file")
        if [ $filesize -eq 0 ]; then
          echo "File must not be empty: $file"
          exit 1
        fi
      }
      findFileExistsNonEmpty() {
        local searchDir=$1
        local filePattern=$2

        local file="$(find $searchDir -name $filePattern -print -quit)"

        if [ -z $file ]; then
          echo "Couldn't find file \"$filePattern\" in directory \"$searchDir\"."
          exit 1
        fi

        local filesize=$(command stat --format '%s' "$file")
        if [ $filesize -eq 0 ]; then
          echo "File must not be empty: $file"
          exit 1
        fi
      }
      dirExistsEmpty() {
        local dir=$1
        if [ ! -d "$dir" ]; then
          echo "Missing: $dir"
          exit 1
        fi
        if [ "$(ls -A $dir)" ]; then
          echo "Dir should be empty: $dir"
          exit 1
        fi
      }
      dirExists() {
        local dir=$1
        if [ ! -d "$dir" ]; then
          echo "Missing: $dir"
          exit 1
        fi
      }

      ${concatStringsSep "\n" (map (project: ''
        pkga_basedir="${project.hsPkgs.pkga.coverageReport}/share/hpc/vanilla"
        findFileExistsNonEmpty $pkga_basedir/mix/pkga-0.1.0.0* "PkgA.mix"
        dirExists "$pkga_basedir/tix/pkga-0.1.0.0"
        dirExists "$pkga_basedir/html/pkga-0.1.0.0"
  
        pkgb_basedir="${project.hsPkgs.pkgb.coverageReport}/share/hpc/vanilla"
        testTix="$pkgb_basedir/tix/pkgb-test-tests${crossSuffix}-0.1.0.0-check${crossSuffix}/tests${exeExt}.tix"
        libTix="$pkgb_basedir/tix/pkgb-0.1.0.0/pkgb-0.1.0.0.tix"
        fileExistsNonEmpty "$testTix"
        fileExistsNonEmpty "$libTix"
        findFileExistsNonEmpty $pkgb_basedir/mix/pkgb-0.1.0.0* "ConduitExample.mix"
        findFileExistsNonEmpty $pkgb_basedir/mix/pkgb-0.1.0.0* "PkgB.mix"
        fileExistsNonEmpty "$pkgb_basedir/html/pkgb-0.1.0.0/hpc_index.html"
  
        filesizeTestsTix=$(command stat --format '%s' "$testTix")
        filesizeLibTix=$(command stat --format '%s' "$libTix")
        if (( filesizeTestsTix <= filesizeLibTix )); then
          echo "Filesize of \"$testTix\" ($filesizeTestsTix) should be greather than that of \"$libTix\" ($filesizeLibTix). Did you forget to exclude test modules when creating \"$libTix\"?"
          exit 1
        fi

        project_basedir="${project.projectCoverageReport}/share/hpc/vanilla"
        fileExistsNonEmpty "$project_basedir/html/index.html"
        dirExists "$project_basedir/html/pkga-0.1.0.0-inplace"
        dirExists "$project_basedir/html/pkgb-0.1.0.0-inplace"
        findFileExistsNonEmpty "$project_basedir/mix/" "PkgA.mix"
        findFileExistsNonEmpty "$project_basedir/mix/" "PkgB.mix"
        findFileExistsNonEmpty "$project_basedir/mix/" "ConduitExample.mix"
        dirExists "$project_basedir/tix/all"
        fileExistsNonEmpty "$project_basedir/tix/all/all.tix"
        dirExists "$project_basedir/tix/pkga-0.1.0.0-inplace"
        dirExists "$project_basedir/tix/pkgb-0.1.0.0-inplace"
        fileExistsNonEmpty "$project_basedir/tix/pkgb-0.1.0.0-inplace/pkgb-0.1.0.0-inplace.tix"
        dirExists "$project_basedir/tix/pkgb-test-tests${crossSuffix}-0.1.0.0-check${crossSuffix}"
        fileExistsNonEmpty "$project_basedir/tix/pkgb-test-tests${crossSuffix}-0.1.0.0-check${crossSuffix}/tests${exeExt}.tix"
      '') ([cabalProj] ++ optional (compiler-nix-name == "ghc865") stackProj))}

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit cabalProj stackProj;
    };
  };
})
