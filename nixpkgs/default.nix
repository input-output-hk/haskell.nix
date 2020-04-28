# see ../docs/dev/nixpkgs-pin.md
let
  fetch = jsonFile:
    with builtins;
    let spec = fromJSON (readFile jsonFile);
    in fetchTarball {
      name = "nixpkgs";
      inherit (spec) sha256;
      url = "${spec.url}/archive/${spec.rev}.tar.gz";
    };
in
{
  nixpkgs-2003 = fetch (./. + "/release-20.03.json");
  nixpkgs-1909 = fetch (./. + "/release-19.09.json");
  nixpkgs-default = fetch (./. + "/github.json");
}
