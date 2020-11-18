let
  spdx = import ./parser.nix;
in pkgs:
with builtins;
let
  # For better performance these are not in the
  # let block below (probably helps by increasing
  # the sharing)
  license-map = import ../cabal-licenses.nix pkgs;
  otherLicenseWarning = lic:
    trace "WARNING: license \"${lic}\" not found"
          license-map.LicenseRef-OtherLicense;
in license:
let
  licenses = spdx.compound-expression license;
in if licenses == []
   then otherLicenseWarning license
   else map (lic: license-map.${lic} or (otherLicenseWarning lic))
            (pkgs.lib.unique (head licenses)._1)
