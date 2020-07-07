{ stdenv, writeScript, nix }:

with stdenv.lib;

writeScript "check-path-supprot.sh" ''
  #!${stdenv.shell}

  set -euo pipefail

  export PATH="${makeBinPath [ nix ]}"

  nix-build -E '((import ./. {}).pkgs.haskell-nix.cabalProject { compiler-nix-name = "ghc865"; src = ./test/cabal-simple; }).cabal-simple.components.library'
''
