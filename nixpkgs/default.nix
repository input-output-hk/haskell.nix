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
  nixpkgs-1909 = fetch (./. + "/release-19.09.json");
  nixpkgs-1903 = fetch (./. + "/release-19.03.json");
  nixpkgs-default = fetch (./. + "/github.json");
}
