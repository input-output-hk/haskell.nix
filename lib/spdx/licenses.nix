pkgs:
with builtins; let
  # Support for nixpkgs 19.09 and older where spdx-license-list-data
  # does not exits. TODO remove this once every one is using 20.03 or
  # newer.
  spdx-license-list-data = pkgs.spdx-license-list-data or
    (pkgs.stdenv.mkDerivation rec {
      pname = "spdx-license-list-data";
      version = "3.10";

      src = pkgs.fetchFromGitHub {
        owner = "spdx";
        repo = "license-list-data";
        rev = "v${version}";
        sha256 = "1zza0jrs82112dcjqgkyck2b7hv4kg9s10pmlripi6c1rs37av14";
      };

      phases = [ "unpackPhase" "installPhase" ];

      installPhase = ''
        install -vDt $out/json json/licenses.json
      '';

      meta = {
        description = "Various data formats for the SPDX License List";
        homepage = "https://github.com/spdx/license-list-data";
        license = pkgs.lib.licenses.cc0;
        platforms = pkgs.lib.platforms.all;
      };
    });

  licensesJSON = fromJSON (replaceStrings
      [ "\\u0026" "\\u0027" "\\u003d" ]
      [ "&" "'" "=" ]
      (readFile "${spdx-license-list-data}/json/licenses.json")
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
