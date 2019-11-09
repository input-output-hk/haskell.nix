# see ../docs/dev/nixpkgs-pin.md
{ nixpkgs-pin ? "release-19.09" }:
let
  fetch = jsonFile:
    with builtins;
    let spec = fromJSON (readFile jsonFile);
    in fetchTarball {
      name = "nixpkgs";
      inherit (spec) sha256;
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
    };
in fetch (./. + "/${nixpkgs-pin}.json")
