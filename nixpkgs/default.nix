# see ../docs/dev/nixpkgs-pin.md
{ nixpkgs-pin ? "github", ... }@args:
let
  fetch = jsonFile:
    with builtins;
    let spec = fromJSON (readFile jsonFile);
    in fetchTarball {
      name = "nixpkgs";
      inherit (spec) sha256;
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
    };
in import (fetch (./. + "/${nixpkgs-pin}.json"))
  ((import ../.) // (builtins.removeAttrs args [ "nixpkgs-pin" ]))
