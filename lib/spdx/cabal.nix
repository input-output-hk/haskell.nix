lib:
license: let
  license-map = import ../cabal-licenses.nix lib;
  otherLicenseWarning = lic:
    builtins.trace "WARNING: license \"${lic}\" not found"
                   license-map.LicenseRef-OtherLicense;
  spdx = import ../lib/spdx/parser.nix;
  licenses = spdx.compoundExpression license;
in if licenses == []
   then otherLicenseWarning license
   else map (lic: license-map.${lic} or (otherLicenseWarning lic))
            (lib.unique (head licenses)._1);
