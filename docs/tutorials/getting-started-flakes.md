# Getting started with flakes

This version of the getting started guide is for users who
are using [Nix Flakes](https://nixos.wiki/wiki/Flakes).
The non flakes version of the guide is [here](getting-started.md).

`haskell.nix` can automatically translate your
[Cabal](https://cabal.readthedocs.io/en/latest/cabal-project.html)
or [Stack](https://docs.haskellstack.org/en/stable/README/#quick-start-guide)
project and its dependencies into Nix code.

Assuming you have [Nix](https://nixos.org/download.html) installed, you can
start setting up your project.

## Using `flake init` and `nix`

The following `nix flake init` command creates a template `hello` package containing a `flake.nix` and `nix/hix.nix` file. The project can be used with
regular `nix` tools. This template is defined in the [NixOS/templates repository](https://github.com/NixOS/templates/tree/master/haskell.nix).

```bash
nix flake init --template templates#haskell-nix --impure
# `--impure` is required by `builtins.currentSystem`
nix develop
cabal build
```

To view the contents of the flake run:

```
nix flake show
```

To build a component with nix:

```
nix build .#hello:exe:hello
```

To build and run a component:

```
nix run .#hello:exe:hello
```

## Setting up the binary cache

IMPORTANT: you *must* do this or you *will* build several copies of GHC!

You can configure Nix to use our binary cache, which is pushed to by CI, so should contain the artifacts that you need.

You need to add the following sections to `/etc/nix/nix.conf` or, if you are a trusted user, `~/.config/nix/nix.conf` (if you don't know what a "trusted user" is, you probably want to do the former).

```
trusted-public-keys = [...] hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= [...]
substituters = [...] https://cache.iog.io [...]
```

If you're running NixOS, you need to add/update the following in your `/etc/nixos/configuration.nix` files instead.

```nix
# Binary Cache for Haskell.nix
nix.settings.trusted-public-keys = [
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
nix.settings.substituters = [
  "https://cache.iog.io"
];
```

NixOS-21.11 and older use slightly different settings.

```nix
# Binary Cache for Haskell.nix  
nix.binaryCachePublicKeys = [
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
nix.binaryCaches = [
  "https://cache.iog.io"
];   
```

This can be tricky to get setup properly. If you're still having trouble getting cache hits, consult the corresponding [troubleshooting section](../troubleshooting.md#why-am-i-building-ghc).

## Scaffolding

The following work with `stack.yaml` and `cabal.project` based
projects.

Add `flake.nix`:

```nix
{{#include getting-started-flakes/flake.nix}}
```

> **Note:** Git dependencies
>
> If you have git dependencies in your project, you'll need
> to [calculate sha256 hashes for them](./source-repository-hashes.md).

### Working with a project

Top-level attributes are Haskell packages (incl. dependencies) part of your project.

To build the library component of a package in the project run:

```shell
nix build .#your-package-name:lib:your-package-name
```

There are also other components such as `exe`, `test` and `benchmark`.
To build an executable:

```shell
nix build .#your-package-name:exe:your-exe-name
```

To use the `devShell` provided by the flake run:

```shell
nix develop .
cabal repl your-package-name:lib:your-package-name
cabal build your-package-name
```

To open a shell for use with `stack` see [the following issue](https://github.com/input-output-hk/haskell.nix/issues/689#issuecomment-643832619).

## Going forward

Read through [project](../reference/library.md#project) function reference to see how the API works.

There are a number of things to explore further in the tutorials section.

[haskell.nix]: https://github.com/input-output-hk/haskell.nix
