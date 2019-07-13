with import <nixpkgs> {};
let this = import ./. {}; in
stdenv.mkDerivation rec {
  name = "env";
  env = buildEnv { name = name; paths = buildInputs; };
  buildInputs = [
    this.ghc
    this.cabal-install
    this.nix-tools
  ];
}
