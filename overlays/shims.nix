# Shims to make older versions of nixpkgs work
final: prev: {
  # nixpkgs 19.09 and older does not have runCommandLocal
  runCommandLocal = prev.runCommandLocal or (name: env:
    final.runCommand name ({
      preferLocalBuild = true;
      allowSubstitutes = false;
    } // env));

  # Support for nixpkgs 19.09 and older where spdx-license-list-data
  # does not exits. TODO remove this once every one is using 20.03 or
  # newer.
  spdx-license-list-data = prev.spdx-license-list-data or
    (final.stdenv.mkDerivation rec {
      pname = "spdx-license-list-data";
      version = "3.10";

      src = final.fetchFromGitHub {
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
        license = final.lib.licenses.cc0;
        platforms = final.lib.platforms.all;
      };
    });
}
