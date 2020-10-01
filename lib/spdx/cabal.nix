pkgs:
license: with builtins; let
  license-map = import ../cabal-licenses.nix pkgs;
  otherLicenseWarning = lic:
    trace "WARNING: license \"${lic}\" not found"
          license-map.LicenseRef-OtherLicense;
  spdx = import ./parser.nix;
  licenses = spdx.compound-expression license;
in if licenses == []
   then otherLicenseWarning license
   else map (lic: license-map.${lic} or (otherLicenseWarning lic))
            (pkgs.lib.unique (head licenses)._1)
