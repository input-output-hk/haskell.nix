pkgs:
with builtins; let
  materialized' = ../../materialized + "/spdx-${pkgs.evalPackages.spdx-license-list-data.version}";
  licensesJSON = fromJSON (replaceStrings
      [ "\\u0026" "\\u0027" "\\u003d" ]
      [ "&" "'" "=" ]
      (readFile "${pkgs.evalPackages.haskell-nix.materialize {
        materialized = if pathExists materialized' then materialized' else null;
      } (pkgs.evalPackages.runCommand "spdx-json" {} ''
          mkdir $out
          cp ${pkgs.evalPackages.spdx-license-list-data.json or pkgs.evalPackages.spdx-license-list-data}/json/licenses.json $out
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
