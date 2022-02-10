# A project coverage report is a composition of package coverage
# reports
{ stdenv, pkgs, lib, haskellLib }:

# haskell.nix project
project:
# List of coverage reports to accumulate
coverageReports:

let
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

  ghc = project.pkg-set.config.ghc.package;

  libs = lib.remove null (map (r: r.library) coverageReports);

  writeArr = name: arr: pkgs.writeText name (lib.concatStringsSep "\n" arr);

  mixDirs =
    map
      (l: "${l}/share/hpc/vanilla/mix/${l.identifier.name}-${l.identifier.version}")
      libs;
  mixDirsFile = writeArr "mixdirs" mixDirs;

  srcDirs = map (l: l.srcSubDirPath) libs;
  srcDirsFile = writeArr "srcdirs" srcDirs;

in pkgs.runCommand "project-coverage-report"
  ({ nativeBuildInputs = [ (ghc.buildGHC or ghc) pkgs.buildPackages.zip ];
     LANG = "en_US.UTF-8";
     LC_ALL = "en_US.UTF-8";
  } // lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc") {
    LOCALE_ARCHIVE = "${pkgs.buildPackages.glibcLocales}/lib/locale/locale-archive";
  })
  ''
    function markup() {
      local modulesFile=$1
      local destDir=$2
      local tixFile=$3

      local hpcMarkupCmd=("hpc" "markup" "--destdir=$destDir")
      hpcMarkupCmd+=("--srcdirs-from=${srcDirsFile}")
      hpcMarkupCmd+=("--hpcdirs-from=${mixDirsFile}")
      hpcMarkupCmd+=("--includes-from=$modulesFile")
      hpcMarkupCmd+=("$tixFile")

      echo "''${hpcMarkupCmd[@]}"
      eval "''${hpcMarkupCmd[@]}"
    }

    function findModules() {
      local modulesFile=$1
      local searchDir=$2

      pushd $searchDir
      find ./ -type f \
        -wholename "*.mix" -not -name "Paths*" \
        -exec basename {} \; \
        | sed "s/\.mix$//" \
        >> "$modulesFile"
      popd
    }

    mkdir -p $out/nix-support
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
      cp -Rn $report/share/hpc/vanilla/mix/$identifier $out/share/hpc/vanilla/mix/
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
      echo "report coverage-per-package $out/share/hpc/vanilla/html/index.html" >> $out/nix-support/hydra-build-products

      local markupOutDir="$out/share/hpc/vanilla/html/all"

      mkdir $markupOutDir
      mixModules="$PWD/mix-modules"
      touch "$mixModules"
      findModules "$mixModules" "$out/share/hpc/vanilla/mix/"

      markup "$mixModules" "$markupOutDir" "$tixFile"

      echo "report coverage $markupOutDir/hpc_index.html" >> $out/nix-support/hydra-build-products
      ( cd $out/share/hpc/vanilla/html ; zip -r $out/share/hpc/vanilla/html.zip . )
      echo "file zip $out/share/hpc/vanilla/html.zip" >> $out/nix-support/hydra-build-products
    fi
  ''
