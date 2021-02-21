{ stdenv, lib, writeScript, coreutils, gnutar, gzip, nix }:

with lib;

writeScript "check-path-supprot.sh" ''
  #!${stdenv.shell}

  set -euo pipefail

  export PATH="${makeBinPath [ coreutils gnutar gzip nix ]}"

  nix-build -E '((import ./. {}).pkgs.haskell-nix.cabalProject { compiler-nix-name = "ghc865"; src = ./test/cabal-simple; }).cabal-simple.components.library'
''
