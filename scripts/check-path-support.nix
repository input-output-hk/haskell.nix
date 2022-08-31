{ stdenv, lib, writeScript, coreutils, gnutar, gzip, nix, gitMinimal, compiler-nix-name }:

with lib;

writeScript "check-path-supprot.sh" ''
  #!${stdenv.shell}

  set -euo pipefail

  export PATH="${makeBinPath [ coreutils gnutar gzip nix gitMinimal ]}"

  nix-build -E '((import ./. {}).pkgs.haskell-nix.cabalProject { compiler-nix-name = "${compiler-nix-name}"; src = ./test/cabal-simple; }).cabal-simple.components.library'
''
