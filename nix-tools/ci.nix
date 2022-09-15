builtins.mapAttrs (k: _v:
  let
    pkgs = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/57d58888d91f978fccd8648a05fe79dff71d2384.tar.gz") {
        nixpkgs = builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/2255f292063ccbe184ff8f9b35ce475c04d5ae69.tar.gz";
        nixpkgsArgs = { system = k; };
      };
    inherit (pkgs.haskell-nix.cabalProject { src = ./.; }) nix-tools;
  in
  pkgs.recurseIntoAttrs {
    # These two attributes will appear in your job for each platform.
    nix-tools-exes = pkgs.recurseIntoAttrs nix-tools.components.exes;
  }
) {
  x86_64-linux = {};


  # Uncomment to test build on macOS too
  # x86_64-darwin = {};
}
