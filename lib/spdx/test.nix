let spdx = import ./parser.nix;
in {
  idstringF = [ (spdx.idstring "$@#!$") ];
  idstringP = [ (spdx.idstring "blah") ];

  license-refF = [ (spdx.license-ref "LicenseRef-$@#!$") ];
  license-refP = [ (spdx.license-ref "LicenseRef-blah")
                   (spdx.license-ref "DocumentRef-beep:LicenseRef-boop")
                 ];

  simple-expressionF = [ (spdx.simple-expression "$@#!$") ];
  simple-expressionP = [
    (spdx.simple-expression "blah")
    (spdx.simple-expression "blah+")
    (spdx.simple-expression "LicenseRef-blah")
    (spdx.simple-expression "DocumentRef-beep:LicenseRef-boop")
  ];

  compound-expressionF = [ (spdx.compound-expression "$@#!$") ];
  compound-expressionP = [
    (spdx.compound-expression "blah")
    (spdx.compound-expression "blah+")
    (spdx.compound-expression "LicenseRef-blah")
    (spdx.compound-expression "DocumentRef-beep:LicenseRef-boop")
    (spdx.compound-expression "(blah)")
    (spdx.compound-expression "beep OR boop")
    (spdx.compound-expression "beep AND boop")
    (spdx.compound-expression "beep WITH boop")
    (spdx.compound-expression "beep AND (boop OR blap)")
    (spdx.compound-expression "(beep AND ((boop OR ((blap)))))")
  ];
}
