# Getting started

`haskell.nix` can automatically translate your Cabal or Stack project and its dependencies into Nix code.

This tutorial will teach you how to setup your project

Assuming you have [Nix installed](https://nixos.org/download.html), you can start setting up your project.

## Setting up the Cachix binary cache

You can **avoid compiling GHC and nix-tools** by configuring [Cachix](https://cachix.org)
so you can benefit from the binary cache built by CI:

```bash
$ nix-env -iA cachix -f https://cachix.org/api/v1/install
$ cachix use iohk
```

## Scaffolding

The following configuration will use `stack.yaml` if it exists,
otherwise fallback to `cabal.project`.

Add `default.nix`:

```nix
{ haskellCompiler ? "ghc865"

# Fetch the latest haskell.nix and import its default.nix
, haskellNix ? import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI, hence
# you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

# haskell.nix provides some arguments to be passed to nixpkgs, including some patches
# and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs

# 'cleanGit' cleans a source directory based on the files known by git
, src ? pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  }
, hasStack ? builtins.pathExists (./. + "/cabal.project")
, hasCabalProject ? builtins.pathExists (./. + "/stack.yaml")
}:

assert (if hasStack && hasCabalProject then throw "This project has both stack.yaml and cabal.project. Edit default.nix to pick the one you'd like to use." else true);

if hasStack
then pkgs.haskell-nix.stackProject
  { inherit src;
  }
else pkgs.haskell-nix.cabalProject
  { inherit src;
    compiler-nix-name = haskellCompiler;
  }
```

!!! note "git dependencies"
    If you have git dependencies in your project, you'll need
    to [calculate sha256 hashes for them](./source-repository-hashes.md).

### Working with a project

Top-level attributes are Haskell packages (incl. dependencies) part of your project.

To build the library component of a package in the project run:

```shell
nix-build -A your-package-name.components.library
```

There are also other components such as `exes`, `tests`, `benchmarks` and `all`.
To build an executable:

```shell
nix-build -A your-package-name.components.exes.your-exe-name
```

To open a shell for use with `cabal` run:

```shell
nix-shell -A shellFor
cabal new-repl your-package-name:library:your-package-name
cabal new-build your-package-name
```

To open a shell for use with `stack` see [the following issue](https://github.com/input-output-hk/haskell.nix/issues/689#issuecomment-643832619).

### Pinning the [Haskell.nix][] version

For simplicity's sake we will use `fetchTarball` for the examples in
this documentation. This will always get the latest version, and is
similar to an auto-updating Nix channel.

However, in your own project, you may wish to pin [Haskell.nix][] (as
you would pin Nixpkgs). This will make your builds reproducible, more
predictable, and faster (because the fixed version is cached).

One way of doing this is to use `nix-prefetch-git` to get a JSON file
with a revision and SHA-256 hash of [Haskell.nix][].

```
$ nix-prefetch-git --quiet https://github.com/input-output-hk/haskell.nix | tee haskell-nix-src.json
{
  "url": "https://github.com/input-output-hk/haskell.nix",
  "rev": "f1a94a4c82a2ab999a67c3b84269da78d89f0075",
  "date": "2019-06-05T01:06:12+00:00",
  "sha256": "0ggxsppjlb6q6a83y12cwgrdnqnw1s128rpibgzs5p1966bdfqla",
  "fetchSubmodules": false
}
```

(The `tee` command is just to show you the result.)
Use the following expression to import that version:

```nix
{ nixpkgs ? <nixpkgs> }:

let
  spec = builtins.fromJSON (builtins.readFile ./haskell-nix-src.json);
  haskell-nix-src = (import nixpkgs {}).fetchgit {
    name = "haskell-lib";
    inherit (spec) url rev sha256 fetchSubmodules;
  };
in
  import nixpkgs (import haskell-nix-src)
```

There are other possible schemes for pinning. See
[`haskell.nix/lib/fetch-external.nix`](https://github.com/input-output-hk/haskell.nix/blob/master/lib/fetch-external.nix),
the [niv](https://github.com/nmattia/niv) tool, or the Nix Flakes
proposal.

# Going forward

There are a number of things to explore further in the tutorials section.

[haskell.nix]: https://github.com/input-output-hk/haskell.nix