# The following collects coverage information from a set of given "checks" and
# provides a coverage report showing how those "checks" cover a set of given
# "mixLibraries".
{ stdenv, lib, haskellLib, pkgs }:

# Name of the coverage report, which should be unique.
{ name
# List of check derivations that generate coverage.
, checks ? []
# List of libraries to include in the coverage report. If one of the above
# checks generates coverage for a particular library, coverage will only
# be included if that library is in this list.
, mixLibraries ? []
# Hack for project-less projects.
, ghc ? if mixLibraries == [] then null else (lib.head mixLibraries).project.pkg-set.config.ghc.package
}:

let
  mixDir = l: "${l}/share/hpc/vanilla/mix/${l.identifier.name}-${l.identifier.version}";
  mixDirs = map mixDir mixLibraries;

  srcDirs = map (l: l.srcSubDirPath) mixLibraries;

in pkgs.runCommand (name + "-coverage-report")
  ({nativeBuildInputs = [ (ghc.buildGHC or ghc) pkgs.buildPackages.zip ];
    passthru = {
      inherit name checks mixLibraries;
    };
    # HPC will fail if the Haskell file contains non-ASCII characters,
    # unless our locale is set correctly. This has been fixed, but we
    # don't know what version of HPC we will be using, hence we should
    # always use the workaround.
    # https://gitlab.haskell.org/ghc/ghc/-/issues/17073
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
  } // lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc") {
    LOCALE_ARCHIVE = "${pkgs.buildPackages.glibcLocales}/lib/locale/locale-archive";
  })
  ''
    mkdir -p $out/nix-support
    mkdir -p $out/share/hpc/vanilla/mix/
    mkdir -p $out/share/hpc/vanilla/tix/${name}
    mkdir -p $out/share/hpc/vanilla/html/${name}

    local srcDirArgs=$(mktemp)
    ${lib.concatStringsSep "\n" (map (srcDir: ''
        echo --srcdir=${srcDir} >> $srcDirArgs
      '') srcDirs)
    }
    local mixDirArgs=$(mktemp)
    ${ # Copy out mix files used for this report
      lib.concatStrings (map (mixDir: ''
        local dir=${mixDir}
        echo --hpcdir=$dir >> $mixDirArgs
        if [ -d $dir ]; then
          cp -R "$dir" $out/share/hpc/vanilla/mix/
        fi
      '') mixDirs)
    }
    local includeArgs=$(mktemp)
    find $out/share/hpc/vanilla/mix/ -type f \
      -wholename "*.mix" -not -name "Paths*" \
      -exec basename {} \; \
      | sed "s/\.mix$//" \
      | sed "s/^.*$/--include=\0/" \
      >> $includeArgs

    local tixFiles=$(mktemp -d)/tixFiles
    ${lib.concatStringsSep "\n" (builtins.map (check: ''
      if [ -d "${check}/share/hpc/vanilla/tix" ]; then
        pushd ${check}/share/hpc/vanilla/tix

        tixFile="$(find . -iwholename "*.tix" -type f -print -quit)"
        local newTixFile=$out/share/hpc/vanilla/tix/${check.name}/"$(basename $tixFile)"

        mkdir -p "$(dirname $newTixFile)"
        # Copy over the tix file verbatim
        cp "$tixFile" "$newTixFile"

        # Add the tix file to our list
        echo $newTixFile >> $tixFiles

        # Create a coverage report for *just that check* affecting any of the
        # "mixLibraries"
        local responseFile=$(mktemp)
        echo markup > $responseFile
        echo '--destdir'=$out/share/hpc/vanilla/html/${check.name}/ >> $responseFile
        cat $srcDirArgs $mixDirArgs $includeArgs >> $responseFile
        echo $newTixFile >> $responseFile

        echo hpc response file:
        cat $responseFile
        hpc @$responseFile

        popd
      fi
    '') checks)
    }

    # Sum tix files to create a tix file with tix information from all tests in
    # the package and markup a HTML report from this info.
    local sumTixFile="$out/share/hpc/vanilla/tix/${name}/${name}.tix"
    local markupOutDir="$out/share/hpc/vanilla/html/${name}"

    # Sum all of our tix files
    if [ -e $tixFiles ]; then
      local responseFile=$(mktemp)
      echo sum > $responseFile
      echo '--union' >> $responseFile
      echo '--output'=$sumTixFile >> $responseFile
      cat $includeArgs >> $responseFile
      cat $tixFiles >> $responseFile

      echo hpc response file:
      cat $responseFile
      hpc @$responseFile
    else
      # If there are no tix files we output an empty tix file so that we can
      # markup an empty HTML coverage report. This is preferable to failing to
      # output a HTML report.
      echo 'Tix []' > $sumTixFile
    fi

    # Markup a HTML report
    local responseFile=$(mktemp)
    echo markup > $responseFile
    echo '--destdir'=$markupOutDir >> $responseFile
    cat $srcDirArgs $mixDirArgs $includeArgs >> $responseFile
    echo $sumTixFile >> $responseFile
    echo hpc response file:
    cat $responseFile
    hpc @$responseFile

    # Provide a HTML zipfile and Hydra links
    ( cd "$markupOutDir" ; zip -r $out/share/hpc/vanilla/${name}-html.zip . )
    echo "report coverage $markupOutDir/hpc_index.html" >> $out/nix-support/hydra-build-products
    echo "file zip $out/share/hpc/vanilla/${name}-html.zip" >> $out/nix-support/hydra-build-products
  ''
