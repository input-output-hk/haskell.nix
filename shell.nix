{ ghc-name ? "ghc8104" }:
let
  inherit (import ./default.nix {}) sources nixpkgsArgs;
  pkgs = import sources.nixpkgs nixpkgsArgs;
in (pkgs.mkShell rec {
  buildInputs = [
    pkgs.haskell-nix.compiler.${ghc-name}
  ];
})
