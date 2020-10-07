{ stdenv, cabal-install, cabalProject', stackProject', recurseIntoAttrs, runCommand, testSrc, compiler-nix-name }:

with stdenv.lib;

let
  projectArgs = {
    src = testSrc "coverage";
    inherit compiler-nix-name;
    modules = [{
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.bytestring-builder.doHaddock = false;

      # Coverage
      packages.pkga.components.library.doCoverage = true;
      packages.pkgb.components.library.doCoverage = true;
    }];
  };

  cabalProj = (cabalProject' projectArgs);
  stackProj = (stackProject' projectArgs);

  exeExt = stdenv.hostPlatform.extensions.executable;

in recurseIntoAttrs ({
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
        findFileExistsNonEmpty "$pkga_basedir/mix/pkga-0.1.0.0/" "PkgA.mix"
        dirExistsEmpty "$pkga_basedir/tix/pkga-0.1.0.0"
        dirExistsEmpty "$pkga_basedir/html/pkga-0.1.0.0"
  
        pkgb_basedir="${project.hsPkgs.pkgb.coverageReport}/share/hpc/vanilla"
        testTix="$pkgb_basedir/tix/pkgb-0.1.0.0/tests${exeExt}/tests${exeExt}.tix"
        libTix="$pkgb_basedir/tix/pkgb-0.1.0.0/pkgb-0.1.0.0.tix"
        fileExistsNonEmpty "$testTix"
        fileExistsNonEmpty "$libTix"
        findFileExistsNonEmpty "$pkgb_basedir/mix/pkgb-0.1.0.0/" "ConduitExample.mix"
        findFileExistsNonEmpty "$pkgb_basedir/mix/pkgb-0.1.0.0/" "PkgB.mix"
        fileExistsNonEmpty "$pkgb_basedir/html/pkgb-0.1.0.0/hpc_index.html"
  
        filesizeTestsTix=$(command stat --format '%s' "$testTix")
        filesizeLibTix=$(command stat --format '%s' "$libTix")
        if (( filesizeTestsTix <= filesizeLibTix )); then
          echo "Filesize of \"$testTix\" ($filesizeTestsTix) should be greather than that of \"$libTix\" ($filesizeLibTix). Did you forget to exclude test modules when creating \"$libTix\"?"
          exit 1
        fi

        project_basedir="${project.projectCoverageReport}/share/hpc/vanilla"
        fileExistsNonEmpty "$project_basedir/html/index.html"
        dirExists "$project_basedir/html/pkga-0.1.0.0"
        dirExists "$project_basedir/html/pkgb-0.1.0.0"
        findFileExistsNonEmpty "$project_basedir/mix/" "PkgA.mix"
        findFileExistsNonEmpty "$project_basedir/mix/" "PkgB.mix"
        findFileExistsNonEmpty "$project_basedir/mix/" "ConduitExample.mix"
        dirExists "$project_basedir/tix/all"
        fileExistsNonEmpty "$project_basedir/tix/all/all.tix"
        dirExists "$project_basedir/tix/pkga-0.1.0.0"
        dirExists "$project_basedir/tix/pkgb-0.1.0.0"
        fileExistsNonEmpty "$project_basedir/tix/pkgb-0.1.0.0/pkgb-0.1.0.0.tix"
        dirExists "$project_basedir/tix/pkgb-0.1.0.0/tests${exeExt}"
        fileExistsNonEmpty "$project_basedir/tix/pkgb-0.1.0.0/tests${exeExt}/tests${exeExt}.tix"
      '') [ stackProj ])}

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit cabalProj stackProj;
    };
  };
})
