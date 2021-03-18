pkgs:
with builtins; let
  licensesJSON = fromJSON (replaceStrings
      [ "\\u0026" "\\u0027" "\\u003d" ]
      [ "&" "'" "=" ]
      (readFile "${pkgs.evalPackages.spdx-license-list-data}/json/licenses.json")
    );
  dropFour = s: substring 0 (stringLength s - 4) s;
  toSpdx = lic: with lic;
            { spdxId = licenseId
            ; shortName = licenseId
            ; fullName = name
            ; url = dropFour detailsUrl + "html"
            ; free = isOsiApproved
            ;
            };
  toNamedValue = lic: { name = lic.spdxId; value = lic; };
in

listToAttrs (map (l: toNamedValue (toSpdx l)) licensesJSON.licenses)
