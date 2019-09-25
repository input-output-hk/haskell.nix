#! /usr/bin/env nix-shell
#! nix-shell -I "nixpkgs=channel:nixos-19.03" --pure -i bash -p nix cabal-install ghc git nix-prefetch-git

export NIX_PATH="nixpkgs=channel:nixos-19.03"
index_state="2019-09-15T00:00:00Z"
expected_hash="1qgk0dnsq2j7sa5ww9vv8ikj0cxc4b1y87z22a1mvizjd8h5nlw8"

set -euo pipefail

# This file can interfere with the build.
# https://github.com/input-output-hk/haskell.nix/issues/57
rm -f .nix-tools.cache

echo "+++ Cabal configure"
cabal new-update "hackage.haskell.org,$index_state"
cabal new-configure

echo
echo "+++ Run stable version of plan-to-nix"
nix build -f https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz nix-tools -o nt
./nt/bin/plan-to-nix --output .buildkite/nix1 --plan-json dist-newstyle/cache/plan.json

echo
echo "+++ Build project"
nix build -f .buildkite/nix1 nix-tools.components.exes --no-link

echo
echo "+++ Run tests"
echo "There are no tests -- https://github.com/input-output-hk/haskell.nix/issues/51"

echo
echo "+++ Add runtime dependencies to PATH"

nix build -f channel:nixos-19.03 nix-prefetch-scripts -o nix-prefetch-scripts
nix build -f channel:nixos-19.03 git -o git
export PATH="$PWD/nix-prefetch-scripts/bin:$PWD/git/bin:$PATH"

echo
echo "+++ Run plan-to-nix again"

# This file can interfere with the build.
# https://github.com/input-output-hk/haskell.nix/issues/57
rm -f .nix-tools.cache

nix build -f .buildkite/nix1 nix-tools.components.exes.plan-to-nix
./result/bin/plan-to-nix --output .buildkite/nix2 --plan-json dist-newstyle/cache/plan.json

echo
echo "+++ Build project"

nix build -f .buildkite/nix2 nix-tools.components.exes --no-link

echo
echo "--- Test index file truncation"

find /nix/store/ -name "*-00-index.tar.gz" -exec nix-store --delete {} \;
nix build -f test/truncate-index.nix --no-link \
    --arg nix-tools-path ./.buildkite/nix2  \
    --argstr index-state "$index_state" \
    --argstr hash "$expected_hash" \
    indexTruncated
