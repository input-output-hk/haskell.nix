{ stdenv, lib, haskellLib, pkgs }:

# Name of the coverage report, which should be unique
{ name
# Library to check coverage of
, library
# List of check derivations that generate coverage
, checks
# List of other libraries to include in the coverage report. The
# default value if just the derivation provided as the `library`
# argument. Use a larger list of libraries if you would like the tests
# of one local package to generate coverage for another.
, mixLibraries ? [library]
# hack for project-less projects
, ghc ? library.project.pkg-set.config.ghc.package
}:

let
  toBashArray = arr: "(" + (lib.concatStringsSep " " arr) + ")";

  mixDir = l: "${l}/share/hpc/vanilla/mix/${l.identifier.name}-${l.identifier.version}";
  mixDirs = map mixDir mixLibraries;

  srcDirs = map (l: l.src.outPath) mixLibraries;

in pkgs.runCommand (name + "-coverage-report")
  ({ buildInputs = [ ghc ];
    passthru = {
      inherit name library checks;
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

      local hpcSumCmd=("hpc" "sum" "--union" "--output=$outFile")

      for module in "''${includedModules[@]}"; do
        hpcSumCmd+=("--include=$module")
      done

      for tixFile in "''${tixFs[@]}"; do
        hpcSumCmd+=("$tixFile")
      done

      echo "''${hpcSumCmd[@]}"
      eval "''${hpcSumCmd[@]}"
    }

    function findModules() {
      local searchDir=$2
      local pattern=$3

      pushd $searchDir
      mapfile -d $'\0' $1 < <(find ./ -type f \
        -wholename "$pattern" -not -name "Paths*" \
        -exec basename {} \; \
        | sed "s/\.mix$//" \
        | tr "\n" "\0")
      popd
    }

    local mixDirs=${toBashArray mixDirs}

    mkdir -p $out/share/hpc/vanilla/mix/${name}
    mkdir -p $out/share/hpc/vanilla/tix/${name}
    mkdir -p $out/share/hpc/vanilla/html/${name}

    # Copy over mix files verbatim
    for dir in "''${mixDirs[@]}"; do
      if [ -d "$dir" ]; then
        cp -R "$dir"/* $out/share/hpc/vanilla/mix/${name}
      fi
    done

    local srcDirs=${toBashArray srcDirs}
    local allMixModules=()
    local pkgMixModules=()

    # The behaviour of stack coverage reports is to provide tix files
    # that include coverage information for every local package, but
    # to provide HTML reports that only include coverage info for the
    # current package. We emulate the same behaviour here. If the user
    # includes all local packages in the mix libraries argument, they
    # will get a coverage report very similar to stack.

    # All mix modules
    findModules allMixModules "$out/share/hpc/vanilla/mix/${name}" "*.mix"
    # Only mix modules corresponding to this package
    findModules pkgMixModules "$out/share/hpc/vanilla/mix/${name}" "*${name}*/*.mix"

    # For each test
    local tixFiles=()
    ${lib.concatStringsSep "\n" (builtins.map (check: ''
      if [ -d "${check}/share/hpc/vanilla/tix" ]; then
        pushd ${check}/share/hpc/vanilla/tix

        tixFile="$(find . -iwholename "*.tix" -type f -print -quit)"
        local newTixFile=$out/share/hpc/vanilla/tix/${name}/"$tixFile"

        mkdir -p "$(dirname $newTixFile)"
        # Copy over the tix file verbatim
        cp "$tixFile" "$newTixFile"

        # Add the tix file to our list
        tixFiles+=("$newTixFile")

        # Create a coverage report for *just that test*
        markup srcDirs mixDirs pkgMixModules "$out/share/hpc/vanilla/html/${name}/${check.exeName}/" "$newTixFile"

        popd
      fi
    '') checks)
    }

    # Sum tix files to create a tix file with all relevant tix
    # information and markup a HTML report from this info.
    if (( "''${#tixFiles[@]}" > 0 )); then
      local sumTixFile="$out/share/hpc/vanilla/tix/${name}/${name}.tix"
      local markupOutDir="$out/share/hpc/vanilla/html/${name}"

      # Sum all of our tix file, including modules from any local package
      sumTix allMixModules tixFiles "$sumTixFile"

      # Markup a HTML report, included modules from only this package
      markup srcDirs mixDirs pkgMixModules "$markupOutDir" "$sumTixFile"
    fi
  ''
