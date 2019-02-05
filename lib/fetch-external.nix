# Provides a function for fetching a GitHub repo from a JSON spec,
# overridable with the given entry on the NIX_PATH.

let
  overrideWith = import ./override-with.nix;
in
  { name, specJSON, override }:
  let
    spec = builtins.fromJSON (builtins.readFile specJSON);
  in
    overrideWith override
      (builtins.fetchTarball {
        inherit name;
        url = "${spec.url}/archive/${spec.rev}.tar.gz";
        inherit (spec) sha256;
      })
