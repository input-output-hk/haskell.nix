let
  pkgs = import ./nixpkgs/default.nix (import ./default.nix); 
in pkgs.stdenv.mkDerivation rec {
  name = "env";
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell-nix.ghc
    pkgs.haskell-nix.nix-tools
  ];
}
