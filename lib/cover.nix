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
  toBashArray = arr: "(" + (lib.concatStringsSep " " arr) + ")";

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
    function markup() {
      local -n srcDs=$1
      local -n mixDs=$2
      local -n includedModules=$3
      local destDir=$4
      local tixFile=$5

      local hpcMarkupCmd=("hpc" "markup" "--destdir=$destDir")
      for srcDir in "''${srcDs[@]}"; do
        hpcMarkupCmd+=("--srcdir=$srcDir")
      done

      for mixDir in "''${mixDs[@]}"; do
        hpcMarkupCmd+=("--hpcdir=$mixDir")
      done

      for module in "''${includedModules[@]}"; do
        hpcMarkupCmd+=("--include=$module")
      done

      hpcMarkupCmd+=("$tixFile")

      echo "''${hpcMarkupCmd[@]}"
      eval "''${hpcMarkupCmd[@]}"
    }

    function sumTix() {
      local -n includedModules=$1
      local -n tixFs=$2
      local outFile="$3"

      if (( "''${#tixFs[@]}" > 0 )); then
        local hpcSumCmd=("hpc" "sum" "--union" "--output=$outFile")

        for module in "''${includedModules[@]}"; do
          hpcSumCmd+=("--include=$module")
        done

        for tixFile in "''${tixFs[@]}"; do
          hpcSumCmd+=("$tixFile")
        done

        echo "''${hpcSumCmd[@]}"
        eval "''${hpcSumCmd[@]}"
      else
        # If there are no tix files we output an empty tix file so that we can
        # markup an empty HTML coverage report. This is preferable to failing to
        # output a HTML report.
        echo 'Tix []' > $outFile
      fi
    }

    function findModules() {
      local -n result=$1
      local -n searchDirs=$2
      local pattern=$3

      for dir in "''${searchDirs[@]}"; do
        pushd $dir
        local temp=()
        mapfile -d $'\0' temp < <(find ./ -type f \
          -wholename "$pattern" -not -name "Paths*" \
          -exec basename {} \; \
          | sed "s/\.mix$//" \
          | tr "\n" "\0")
        result+=("''${temp[@]}")
        popd
      done
    }

    mkdir -p $out/nix-support
    mkdir -p $out/share/hpc/vanilla/mix/
    mkdir -p $out/share/hpc/vanilla/tix/${name}
    mkdir -p $out/share/hpc/vanilla/html/${name}

    local srcDirs=${toBashArray srcDirs}
    local mixDirs=${toBashArray mixDirs}

    # Copy out mix files used for this report
    for dir in "''${mixDirs[@]}"; do
      if [ -d "$dir" ]; then
        cp -R "$dir" $out/share/hpc/vanilla/mix/
      fi
    done

    local mixModules=()
    # Mix modules for all packages in "mixLibraries"
    findModules mixModules mixDirs "*.mix"

    # We need to make a distinction between library "exposed-modules" and
    # "other-modules" used in test suites:
    #  - "exposed-modules" are addressed as "$library-$version-$hash/module"
    #  - "other-modules" are addressed as "module"
    #
    # This complicates the code required to find the mix modules. For a given mix directory:
    #
    # mix
    # └── ntp-client-0.0.1
    #     └── ntp-client-0.0.1-gYjRsBHUCaHX7ENcjHnw5
    #         ├── Network.NTP.Client.mix
    #         ├── Network.NTP.Client.Packet.mix
    #         └── Network.NTP.Client.Query.mix
    #
    # Iff ntp-client uses "other-modules" in a test suite, both:
    #   - "mix/ntp-client-0.0.1", and
    #   - "mix/ntp-client-0.0.1/ntp-client-0.0.1-gYjRsBHUCaHX7ENcjHnw5"
    # need to be provided to hpc as search directories.
    #
    # I'd prefer to just exclude "other-modules", but I can't think of an easy
    # way to do that in bash.
    #
    # Here we expand the search dirs and modify the mix dirs accordingly:
    for dir in "''${mixDirs[@]}"; do
      local otherModulesSearchDirs=()
      # Simply consider any directory with a mix file as a search directory.
      mapfile -d $'\0' otherModulesSearchDirs < <(find $dir -type f \
        -wholename "*.mix" \
        -exec dirname {} \; \
        | uniq \
        | tr "\n" "\0")
      mixDirs+=("''${otherModulesSearchDirs[@]}")
    done

    local tixFiles=()
    ${lib.concatStringsSep "\n" (builtins.map (check: ''
      if [ -d "${check}/share/hpc/vanilla/tix" ]; then
        pushd ${check}/share/hpc/vanilla/tix

        tixFile="$(find . -iwholename "*.tix" -type f -print -quit)"
        local newTixFile=$out/share/hpc/vanilla/tix/${check.name}/"$(basename $tixFile)"

        mkdir -p "$(dirname $newTixFile)"
        # Copy over the tix file verbatim
        cp "$tixFile" "$newTixFile"

        # Add the tix file to our list
        tixFiles+=("$newTixFile")

        # Create a coverage report for *just that check* affecting any of the
        # "mixLibraries"
        markup srcDirs mixDirs mixModules "$out/share/hpc/vanilla/html/${check.name}/" "$newTixFile"

        popd
      fi
    '') checks)
    }

    # Sum tix files to create a tix file with tix information from all tests in
    # the package and markup a HTML report from this info.
    local sumTixFile="$out/share/hpc/vanilla/tix/${name}/${name}.tix"
    local markupOutDir="$out/share/hpc/vanilla/html/${name}"

    # Sum all of our tix files
    sumTix mixModules tixFiles "$sumTixFile"

    # Markup a HTML report
    markup srcDirs mixDirs mixModules "$markupOutDir" "$sumTixFile"

    # Provide a HTML zipfile and Hydra links
    ( cd "$markupOutDir" ; zip -r $out/share/hpc/vanilla/${name}-html.zip . )
    echo "report coverage $markupOutDir/hpc_index.html" >> $out/nix-support/hydra-build-products
    echo "file zip $out/share/hpc/vanilla/${name}-html.zip" >> $out/nix-support/hydra-build-products
  ''
