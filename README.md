# `haskell.nix` is infrastructure for building Haskell packages with Nix

[![](https://badge.buildkite.com/d453edcd29bd2f8f3f3b32c9b7d6777a33773d9671c37a6ccc.svg?branch=master)](https://buildkite.com/input-output-hk/haskell-dot-nix)
[![](https://img.shields.io/buildkite/c8d5a20d3ff0f440f82adb9190b43c16c91e5e47e8adfa867a/master.svg?label=nightly%20updates)](https://buildkite.com/input-output-hk/haskell-dot-nix-nightly-updates)

`haskell.nix` can automatically translate your Cabal or Stack project and
its dependencies into Nix code.  

## [Documentation](https://input-output-hk.github.io/haskell.nix/)

## Getting started

### a) Using `cabal.project`

Add a `default.nix`:

```nix
let 
  # Fetch the latest haskell.nix and import its default.nix
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};

  # haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
  # you will be more likely to get cache hits when using these.
  # But you can also just use your own, e.g. '<nixpkgs>'
  nixpkgsSrc = haskellNix.sources.nixpkgs-2003;

  # haskell.nix provides some arguments to be passed to nixpkgs, including some patches
  # and also the haskell.nix functionality itself as an overlay.
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
, haskellCompiler ? "ghc865"
}:

# 'cabalProject' generates a package set based on a cabal.project (and the corresponding .cabal files)
pkgs.haskell-nix.cabalProject {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit { name = "haskell-nix-project"; src = ./.; };
  compiler-nix-name = haskellCompiler;
}
```

Note that you'll need to add a comment specifying the expected sha256
output for your `source-repository-packages` in your `cabal.project`
file:

```
source-repository-package
  type: git
  location: https://github.com/input-output-hk/iohk-monitoring-framework
  subdir:   plugins/backend-editor
  tag: 4956b32f039579a0e7e4fd10793f65b4c77d9044
  --sha256: 03lyb2m4i6p7rpjqarnhsx21nx48fwk6rzsrx15k6274a4bv0pix
```

### b) Using `stack.yaml`

Add a `default.nix`:

```nix
let 
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc nixpkgsArgs
}:
pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
}
```

To build the library component of a package in the project run:

```shell
nix build -f . your-package-name.components.library
```

To build an executable:

```shell
nix build -f . your-package-name.components.exes.your-exe-name
```

To open a shell for use with `cabal` run:

```shell
nix-shell -A shellFor
cabal new-build your-package-name
cabal new-repl your-package-name:library:your-package-name
```

## Using binary Cache to speed up compilation

To use precompiled binaries you'll need:

- to configure a binary cache by following instructions on [iohk.cachix.org](https://iohk.cachix.org).
- to pin `nixpkgs` according to [nix/sources.json](nix/sources.json).


## Related repos

The `haskell.nix` repository contains the runtime system for building
Haskell packages in Nix. It depends on other repos, which are:

- [nix-tools](https://github.com/input-output-hk/nix-tools) — provides the programs for generating Nix expressions from  Haskell projects.

- [hackage.nix](https://github.com/input-output-hk/hackage.nix) — the latest contents of the [Hackage](https://hackage.haskell.org/) databases, converted to Nix expressions.

- [stackage.nix](https://github.com/input-output-hk/stackage.nix) — all of the [Stackage](https://www.stackage.org/) snapshots, converted to Nix expressions.

## IRC Channel

Join the [#haskell.nix](https://www.irccloud.com/invite?channel=%23haskell.nix&hostname=irc.freenode.net&port=6697&ssl=1) channel on [irc.freenode.net](https://freenode.net/) to get help or discuss
the development of `haskell.nix` and `nix-tools`.
