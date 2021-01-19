pkgs:
let licenses = import spdx/licenses.nix pkgs;
in licenses // {
  # Generic
  LicenseRef-PublicDomain = {
      spdxId = "LicenseRef-PublicDomain";
      shortName = "Public Domain";
      fullName = "This work is dedicated to the Public Domain";
      url = "https://wikipedia.org/wiki/Public_domain";
      free = true;
    };
  LicenseRef-OtherLicense = {
      spdxId = "LicenseRef-OtherLicense";
      shortName = "Other License";
      fullName = "Unidentified Other License";
      url = "https://spdx.github.io/spdx-spec/appendix-IV-SPDX-license-expressions/";
    };
  NONE = null;
}
