let
  inherit (import ./default.nix {}) sources nixpkgsArgs;
  pkgs = import sources.nixpkgs-default nixpkgsArgs;
in pkgs.stdenv.mkDerivation rec {
  name = "env";
  env = pkgs.buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    pkgs.cabal-install
    pkgs.haskell-nix.ghc
    pkgs.haskell-nix.nix-tools
  ];
}
