#! /usr/bin/env bash

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
pwd
nix-shell --pure --run ""
nix-shell --pure shell-hoogle.nix --run ""
nix-shell --pure shell-package.nix --run ""
nix-shell --pure shell-stackage.nix --run ""
popd

## Bumping Hackage and Stackage snapshots
pushd hackage-stackage
nix build --accept-flake-config
popd

## Handling git repositories in projects
pushd source-repository-hashes
nix build --accept-flake-config
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
nix build --accept-flake-config
popd

popd
