#! /usr/bin/env bash

# Without this, a failing example is invisible: every command below runs
# unconditionally and the script exits with the status of the last one, so the
# `docs` CI job reported success while almost every example in it was failing.
# A test that cannot fail is worse than no test.
set -euo pipefail

# Tutorials
pushd tutorials

## Getting started
pushd getting-started
nix-build -A hsPkgs.hello.components.exes.hello
nix-shell --pure --run "cabal build hello"
popd

## Getting started with flakes
pushd getting-started-flakes
nix build --accept-flake-config
nix develop . --accept-flake-config -c cabal build hello
popd

## Development
pushd development
nix-shell --pure --run ""
nix-shell --pure shell-hoogle.nix --run ""
nix-shell --pure shell-package.nix --run ""
nix-shell --pure shell-stackage.nix --run ""
popd

## Bumping Hackage and Stackage snapshots
pushd hackage-stackage
# A plain `default.nix`, not a flake -- `nix build` here would resolve the
# repository's own flake instead and ask it for `packages.<system>.default`.
nix-build --no-out-link
popd

## Handling git repositories in projects
pushd source-repository-hashes
# Instantiate rather than build: constructing the plan is what exercises the
# `sha256map` this example is about, and building pandoc 2.9.2.1 with GHC
# 8.10.7 would add hours to the job for no extra coverage.
nix-instantiate
popd

# TODO
# - CleanGit
# - Content addressed derivations
# - Coverage
# - Cross compilation
# - Materialization

popd

# `template/iohk-nix` is deliberately not built here.  That page is four
# `{{#include}}` snippets with no project behind them: there are no sources,
# and `nix/pkgs.nix` reads a `.stack-pkgs.nix` the directory does not contain.
# `nix-build` there evaluates to an attribute set holding no derivations, so it
# exits 0 having built nothing -- a green light for an untested page.
