let
  haskell = import ./default.nix {};
in {
  inherit (haskell) nix-tools;
  tests = import ./test { inherit haskell; };
}
