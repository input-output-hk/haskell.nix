pkgs:
with builtins; let
  spdxJson = pkgs.evalPackages.spdx-license-list-data.json or pkgs.evalPackages.spdx-license-list-data;
  materialized' = ../../materialized + "/spdx-${spdxJson.version}";
  licensesJSON = fromJSON (replaceStrings
      [ "\\u0026" "\\u0027" "\\u003d" ]
      [ "&" "'" "=" ]
      (readFile "${pkgs.evalPackages.haskell-nix.materialize {
        materialized = if pathExists materialized' then materialized' else null;
      } (pkgs.evalPackages.runCommand "spdx-json" { inherit (spdxJson) version; } ''
          mkdir $out
          cp ${spdxJson}/json/licenses.json $out
        '')
      }/licenses.json")
    );
  dropFour = s: substring 0 (stringLength s - 4) s;
  toSpdx = lic: with lic;
            { spdxId    = licenseId;
              shortName = licenseId;
              fullName  = name;
              url       = dropFour detailsUrl + "html";
              free      = isOsiApproved || lic.isFsfLibre or false;
            };
  toNamedValue = lic: { name = lic.spdxId; value = lic; };
in

listToAttrs (map (l: toNamedValue (toSpdx l)) licensesJSON.licenses)
