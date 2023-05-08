#! /usr/bin/env nix-shell
#! nix-shell -I "nixpkgs=channel:nixos-22.11" -i bash -p nixUnstable cabal-install ghc git nix-prefetch-git cacert

# This file uses nixpkgs 22.11 to make our GHA runners happy
# Using `nixpkgs-unstable` currently results in:
#   version `GLIBCXX_3.4.30' not found

# The `nix-shell` is set to run without `--pure`.
# It is possible to use `--pure` if we need to, but it requires setting these.
#  export LANG=en_US.UTF-8
#  export LC_ALL=en_US.UTF-8
#  export LOCALE_ARCHIVE=/run/current-system/sw/lib/locale/locale-archive

export NIX_PATH="nixpkgs=channel:nixos-22.11"
index_state="2020-01-10T00:00:00Z"
expected_hash="0z2jc4fibfxz88pfgjq3wk5j3v7sn34xkwb8h60hbwfwhhy63vx6"

set -euo pipefail

# This file can interfere with the build.
# https://github.com/input-output-hk/haskell.nix/issues/57
rm -f .nix-tools.cache

echo "+++ Cabal update"
cabal new-update

echo
echo "+++ Run stable version of make-install-plan and plan-to-nix"
nix build --impure --expr '(let haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}; in (import haskellNix.sources.nixpkgs-2211 haskellNix.nixpkgsArgs).haskell-nix.nix-tools.ghc8107)' -o nt
./nt/bin/make-install-plan
rm -rf .buildkite/nix1
./nt/bin/plan-to-nix --output .buildkite/nix1 --plan-json dist-newstyle/cache/plan.json
mv dist-newstyle/cabal-files .buildkite/nix1/cabal-files

echo
echo "+++ Build project"
nix build -f .buildkite/nix1 nix-tools.components.exes --no-link

echo
echo "+++ Run tests"
echo "There are no tests -- https://github.com/input-output-hk/haskell.nix/issues/51"

echo
echo "+++ Add runtime dependencies to PATH"

nix build -f channel:nixos-22.11 nix-prefetch-scripts -o nix-prefetch-scripts
nix build -f channel:nixos-22.11 git -o git
export PATH="$PWD/nix-prefetch-scripts/bin:$PWD/git/bin:$PATH"

echo
echo "+++ Run make-install-plan and plan-to-nix again"

# This file can interfere with the build.
# https://github.com/input-output-hk/haskell.nix/issues/57
rm -f .nix-tools.cache

rm -rf dist-newstyle

nix build -f .buildkite/nix1 nix-tools.components.exes.make-install-plan
./result/bin/make-install-plan

nix build -f .buildkite/nix1 nix-tools.components.exes.plan-to-nix
rm -rf .buildkite/nix2
./result/bin/plan-to-nix --output .buildkite/nix2 --plan-json dist-newstyle/cache/plan.json
mv dist-newstyle/cabal-files .buildkite/nix2/cabal-files

echo
echo "+++ Build project"

nix build -f .buildkite/nix2 nix-tools.components.exes --no-link

echo
echo "--- Test index file truncation"

# Build the derivation if it does not exist
nix-build test/truncate-index.nix --no-link \
    --arg nix-tools-path ./.buildkite/nix2  \
    --argstr index-state "$index_state" \
    --argstr hash "$expected_hash" \
    -A indexTruncated

# `--check` it as well in case hash exists already, but code no longer works
nix-build --check test/truncate-index.nix --no-link \
    --arg nix-tools-path ./.buildkite/nix2  \
    --argstr index-state "$index_state" \
    --argstr hash "$expected_hash" \
    -A indexTruncated
