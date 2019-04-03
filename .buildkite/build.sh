#! /usr/bin/env nix-shell
#! nix-shell -I "nixpkgs=channel:nixos-19.03" --pure -i bash -p nix cabal-install ghc nix-prefetch-scripts git

export NIX_PATH="nixpkgs=channel:nixos-19.03"

set -euo pipefail

# This file can interfere with the build.
# https://github.com/input-output-hk/haskell.nix/issues/57
rm -f .nix-tools.cache

echo "+++ Cabal configure"
cabal new-update hackage.haskell.org,2019-03-15T00:00:00Z
cabal new-configure

echo
echo "+++ Run stable version of plan-to-nix"
nix build -f https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz nix-tools -o nt
./nt/bin/plan-to-nix --output nix1 --plan-json dist-newstyle/cache/plan.json

echo
echo "+++ Build project"
nix build -f nix1 nix-tools.components.exes --no-link

echo
echo "+++ Run tests"
echo "There are no tests -- https://github.com/input-output-hk/haskell.nix/issues/51"

echo
echo "+++ Run plan-to-nix again"

# This file can interfere with the build.
# https://github.com/input-output-hk/haskell.nix/issues/57
rm -f .nix-tools.cache

nix build -f nix1 nix-tools.components.exes.plan-to-nix
./result/bin/plan-to-nix --output nix2 --plan-json dist-newstyle/cache/plan.json

echo
echo "+++ Build project"

nix build -f nix2 nix-tools.components.exes --no-link
