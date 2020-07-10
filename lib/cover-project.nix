# A project coverage report is a composition of package coverage
# reports
{ stdenv, pkgs, lib, haskellLib }:

# List of coverage reports to accumulate
coverageReports:

let
  toBashArray = arr: "(" + (lib.concatStringsSep " " arr) + ")";

  # Create table rows for a project coverage index page that look something like:
  #
  # | Package          |
  # |------------------|
  # | cardano-shell    |
  # | cardano-launcher |
  coverageTableRows = coverageReport:
      ''
      <tr>
        <td>
          <a href="${coverageReport.passthru.name}/hpc_index.html">${coverageReport.passthru.name}</href>
        </td>
      </tr>
      '';

  projectIndexHtml = pkgs.writeText "index.html" ''
  <html>
    <head>
      <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    </head>
    <body>
      <table border="1" width="100%">
        <tbody>
          <tr>
            <th>Report</th>
          </tr>

          ${with lib; concatStringsSep "\n" (map coverageTableRows coverageReports)}

        </tbody>
      </table>
    </body>
  </html>
  '';

  ghc =
    if (builtins.length coverageReports) > 0
    then (builtins.head coverageReports).library.project.pkg-set.config.ghc.package or pkgs.ghc
    else pkgs.ghc;

  libs = map (r: r.library) coverageReports;

  projectLibs = map (pkg: pkg.components.library) (lib.attrValues (haskellLib.selectProjectPackages ((lib.head libs).project.hsPkgs)));

  mixDirs =
    map
      (l: "${l}/share/hpc/vanilla/mix/${l.identifier.name}-${l.identifier.version}")
      (projectLibs);

  srcDirs = map (l: l.src.outPath) (projectLibs);

in pkgs.runCommand "project-coverage-report"
  ({ buildInputs = [ghc];
     LANG        = "en_US.UTF-8";
     LC_ALL      = "en_US.UTF-8";
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

    mkdir -p $out/share/hpc/vanilla/tix/all
    mkdir -p $out/share/hpc/vanilla/mix/
    mkdir -p $out/share/hpc/vanilla/html/

    # Find all tix files in each package
    tixFiles=()
    ${with lib; concatStringsSep "\n" (map (coverageReport: ''
      identifier="${coverageReport.name}"
      report=${coverageReport}
      tix="$report/share/hpc/vanilla/tix/$identifier/$identifier.tix"
      if test -f "$tix"; then
        tixFiles+=("$tix")
      fi

      # Copy mix, tix, and html information over from each report
      cp -Rn $report/share/hpc/vanilla/mix/$identifier/* $out/share/hpc/vanilla/mix/
      cp -R $report/share/hpc/vanilla/tix/* $out/share/hpc/vanilla/tix/
      cp -R $report/share/hpc/vanilla/html/* $out/share/hpc/vanilla/html/
    '') coverageReports)}

    if [ ''${#tixFiles[@]} -ne 0 ]; then
      # Create tix file with test run information for all packages
      tixFile="$out/share/hpc/vanilla/tix/all/all.tix"
      hpcSumCmd=("hpc" "sum" "--union" "--output=$tixFile")
      hpcSumCmd+=("''${tixFiles[@]}")
      echo "''${hpcSumCmd[@]}"
      eval "''${hpcSumCmd[@]}"

      # Markup a HTML coverage report for the entire project
      cp ${projectIndexHtml} $out/share/hpc/vanilla/html/index.html

      local markupOutDir="$out/share/hpc/vanilla/html/all"
      local srcDirs=${toBashArray srcDirs}
      local mixDirs=${toBashArray mixDirs}
      local allMixModules=()

      mkdir $markupOutDir
      findModules allMixModules "$out/share/hpc/vanilla/mix/" "*.mix"

      markup srcDirs mixDirs allMixModules "$markupOutDir" "$tixFile"
    fi
  ''
