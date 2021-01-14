# A script for regenerating nix for tests
let
  haskellNix = (import ../default.nix {});
in
{ pkgs ? import nixpkgs nixpkgsArgs
, nixpkgs ? haskellNix.sources.nixpkgs
, nixpkgsArgs ? haskellNix.nixpkgsArgs
}:

with pkgs;

writeScript "regen-tests.sh" ''
  #!${pkgs.runtimeShell}

  set -euo pipefail

  export PATH="${lib.makeBinPath [ coreutils glibc haskell-nix.nix-tools cabal-install haskell-nix.compiler.ghc865 ]}"

  cabal_configure() {
    cabal new-configure \
      --with-compiler ghc-8.6.5 \
      --constraint 'transformers == 0.5.6.2' \
      --constraint 'process == 1.6.5.0'
  }

  plan_nix() {
    plan-to-nix -o . --plan-json dist-newstyle/cache/plan.json
  }

  cabal_nix() {
    cabal-to-nix $1.cabal > $1.nix
  }

  regen() {
    cabal_configure
    plan_nix
    cabal_nix $1
  }

  cd builder-haddock
  regen test-haddock
  cd ..

  cd cabal-22
  regen project
  cd ..

  cd cabal-simple
  regen cabal-simple
  cd ..

  cd cabal-sublib
  regen cabal-sublib
  cd ..

  cd with-packages
  regen test-with-packages
  cd ..

  cd stack-simple
  stack-to-nix -o .
  cd ..

  cd shell-for
  cabal_configure
  plan-to-nix -o . --plan-json dist-newstyle/cache/plan.json --cabal-project cabal.project
  cd ..
''
