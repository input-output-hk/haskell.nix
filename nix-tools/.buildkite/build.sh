#! /usr/bin/env nix-shell
#! nix-shell -I "nixpkgs=channel:nixos-21.11" --pure -i bash -p nixUnstable cabal-install ghc git nix-prefetch-git cacert

export NIX_PATH="nixpkgs=channel:nixos-21.11"
index_state="2020-01-10T00:00:00Z"
expected_hash="0z2jc4fibfxz88pfgjq3wk5j3v7sn34xkwb8h60hbwfwhhy63vx6"

set -euo pipefail

# This file can interfere with the build.
# https://github.com/input-output-hk/haskell.nix/issues/57
rm -f .nix-tools.cache

echo "+++ Cabal configure"
cabal new-update
cabal new-configure

echo
echo "+++ Run stable version of plan-to-nix"
nix build --impure --expr '(let haskellNix = import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}; in (import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs).haskell-nix.nix-tools.ghc8107)' -o nt
./nt/bin/plan-to-nix --output .buildkite/nix1 --plan-json dist-newstyle/cache/plan.json

# Replace currently broken plan-to-nix output
cp .buildkite/fixed.nix .buildkite/nix1/default.nix

echo
echo "+++ Build project"
nix build -f .buildkite/nix1 nix-tools.components.exes --no-link

echo
echo "+++ Run tests"
echo "There are no tests -- https://github.com/input-output-hk/haskell.nix/issues/51"

echo
echo "+++ Add runtime dependencies to PATH"

nix build -f channel:nixos-20.03 nix-prefetch-scripts -o nix-prefetch-scripts
nix build -f channel:nixos-20.03 git -o git
export PATH="$PWD/nix-prefetch-scripts/bin:$PWD/git/bin:$PATH"

echo
echo "+++ Run plan-to-nix again"

# This file can interfere with the build.
# https://github.com/input-output-hk/haskell.nix/issues/57
rm -f .nix-tools.cache

nix build -f .buildkite/nix1 nix-tools.components.exes.plan-to-nix
./result/bin/plan-to-nix --output .buildkite/nix2 --plan-json dist-newstyle/cache/plan.json

# Add module needed to allow Cabal 3.2 to be installed
sed -i -e 's|modules = \[\]|modules = \[{ nonReinstallablePkgs = \[ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base" "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell" "ghcjs-prim" "ghcjs-th" "ghc-boot" "ghc" "Win32" "array" "binary" "bytestring" "containers" "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim" "hpc" "mtl" "parsec" "process" "text" "time" "transformers" "unix" "xhtml" "stm" "terminfo" \]; }\]|' \
  .buildkite/nix2/default.nix

echo
echo "+++ Build project"

nix build -f .buildkite/nix2 nix-tools.components.exes --no-link

echo
echo "--- Test index file truncation"

shopt -s nullglob
for a in /nix/store/*-00-index.tar.gz; do nix-store --delete $a; done
shopt -u nullglob

nix build -f test/truncate-index.nix --no-link \
    --arg nix-tools-path ./.buildkite/nix2  \
    --argstr index-state "$index_state" \
    --argstr hash "$expected_hash" \
    indexTruncated
