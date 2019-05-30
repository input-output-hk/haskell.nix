let
  haskell = import ./default.nix {};
in {
  inherit (haskell) nix-tools source-pins;

  tests = import ./test { inherit haskell; };
}
