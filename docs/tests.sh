#! /usr/bin/env bash

# Tutorials
pushd tutorials

## Getting started
pushd getting-started
nix-build
nix-shell --run "cabal build hello"
popd

## Getting started with flakes
pushd getting-started-flakes
nix build
nix develop . -c cabal build hello
popd

## Development
pushd development
pwd
nix-shell --pure --run "cabal build hello" # FIXME
nix-shell --pure shell-hoogle.nix --run "cabal build hello" # FIXME --run "hoogle server --local"
nix-shell --pure shell-package.nix --run "cabal build hello" # FIXME
nix-shell --pure shell-stackage.nix --run "cabal build hello" # FIXME
popd

## Bumping Hackage and Stackage snapshots
pushd hackage-stackage
nix build
popd

## Handling git repositories in projects
pushd source-repository-hashes
nix build
popd

# TODO
# - CleanGit
# - Content addressed derivations
# - Coverage
# - Cross compilation
# - Materialization

popd
# Templates
pushd template

# IOHK's nix tooling
pushd iohk-nix
nix-build
popd

popd