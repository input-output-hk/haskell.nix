{ haskellNix ? import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}
, nixpkgs ? haskellNix.sources.nixpkgs-default }:

let
  pkgs = import nixpkgs haskellNix.nixpkgsArgs;

  pkgSet = pkgs.haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [{
     nonReinstallablePkgs= [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th"
      "ghc-boot"
      "ghc" "Win32" "array" "binary" "bytestring" "containers"
      "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
      # "ghci" "haskeline"
      "hpc"
      "mtl" "parsec" "process" "text" "time" "transformers"
      "unix" "xhtml"
      # "stm" "terminfo"
     ];
    }];
  };

in
  pkgSet.config.hsPkgs
