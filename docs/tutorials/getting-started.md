# Getting started

`haskell.nix` can automatically translate your [Cabal](https://cabal.readthedocs.io/en/latest/cabal-project.html) or [Stack](https://docs.haskellstack.org/en/stable/README/#quick-start-guide) project and its dependencies into Nix code.

The first steps are to ensure you have [Nix](https://nixos.org/download.html) installed and [the binary cache set up](#setting-up-the-binary-cache) on your machine.

Then you have the choice to set-up your project using [the experimental Flake feature](#create-a-project-using-flakes) or [Niv](#create-a-project-using-niv), which are 2 ways to pin `nixpkgs` with Nix.

If you have an existing Haskell project that you want to build with `haskell.nix`. you might [prefer to use `hix`](#getting-started-with-hix).

## Setting up the binary cache

**IMPORTANT:** you _must_ do this or you _will_ build several copies of GHC!

You can configure Nix to use our binary cache, which is pushed to by CI, so should contain the artifacts that you need.

You need to add the following sections to `/etc/nix/nix.conf` or, if you are a trusted user, `~/.config/nix/nix.conf` (if you don't know what a "trusted user" is, you probably want to do the former). `[...]` denote any existing entries.
```
extra-trusted-public-keys = [...] hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= [...]
extra-substituters = [...] https://cache.iog.io [...]
```

If you're running NixOS, you need to add/update the following in your `/etc/nixos/configuration.nix` files instead.
```nix
# Binary Cache for haskell.nix
nix.settings.trusted-public-keys = [
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
nix.settings.substituters = [
  "https://cache.iog.io"
];
```

NixOS-21.11 and older use slightly different settings.
```nix
# Binary Cache for haskell.nix  
nix.binaryCachePublicKeys = [
  "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
];
nix.binaryCaches = [
  "https://cache.iog.io"
];   
```

This can be tricky to get setup properly. If you're still having trouble getting cache hits, consult the corresponding [troubleshooting section](../troubleshooting.md#why-am-i-building-ghc).

## Create a project using Flakes

This section assumes you choose to uses the experimental flakes features, and so that you have added `experimental-features = [ "nix-command" "flakes" ];` in your Nix configuration. You can look at [the Wiki](https://nixos.wiki/wiki/Flakes) for more instructions. If you don't want to set up a Flakes project, [skip to the Niv section](#create-a-project-using-niv).

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

## Create a project using Niv

[Niv](https://github.com/nmattia/niv) is a command line tool for keeping track of Nix project dependencies. You don't need it if you [set up your project with Flakes](#create-a-project-using-flakes), and so you can skip this section.

This guide assumes that the `sources.haskellNix` will be set to point a pinned copy of the `haskell.nix` github repo. One easy way to do this is to use Niv. If you prefer not to use Niv another option is described in the in the following "[Using `haskell.nix` without Niv](#using-1-without-Niv)" section of this document.

After installing niv you can initialize niv and pin the latest `haskell.nix` commit by running the following in the root directory of the project:
```shell
niv init
niv add input-output-hk/haskell.nix -n haskellNix
```

Then when you want to update to the latest version of `haskellNix` use:
```shell
niv update haskellNix
```

### Using `haskell.nix` without Niv

If you would prefer not to use niv you can replace `sources = import ./nix/sources.nix {};` in the examples with:
```nix
let sources = {
  haskellNix = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz";
};
```

The `fetchTarball` call above will always get the latest version, and is similar to an auto-updating Nix channel.

However, in your own project, you may wish to pin `haskell.nix` (as you would pin Nixpkgs). This will make your builds reproducible, more predictable, and faster (because the fixed version is cached).

Straightforward way of doing this is to change the branch name to a revision.
```nix
let sources = {
  haskellNix = builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/f1a94a4c82a2ab999a67c3b84269da78d89f0075.tar.gz";
};
```

There are other possible schemes for pinning. See [Bumping Hackage and Stackage snapshots](./hackage-stackage.md) and [Nix tutorial on reproducibility using pinning](https://nix.dev/tutorials/towards-reproducibility-pinning-nixpkgs.html).

## Scaffolding

The following code could be capy-pasted and will work with `stack.yaml` and `cabal.project` based projects.

For a Flakes-based project, use a `flake.nix`:
```nix
{{#include getting-started-flakes/flake.nix}}
```

For a legacy (Nix) Nix project rather, use a `default.nix`:
```nix
{{#include getting-started/default.nix}}
```

> **Note:** Git dependencies
> 
> If you have git dependencies in your project, you'll need to [calculate sha256 hashes for them](./source-repository-hashes.md).

## Working with a project

Top-level attributes are Haskell packages (incl. dependencies) part of your project.

This section will show side by side the commands using Flakes experimental `new-command` API and legacy Nix commands.

To build the library component of a package in the project run:

* With flakes experimental commands:
  ```shell
  nix build .#your-package-name:lib:your-package-name
  ```
* With Nix legacy commands:
  ```shell
  nix-build -A your-package-name.components.library
  ```

There are also other components such as `exes`, `tests`, `benchmarks` and `all`.
To build an executable:

* With flakes experimental commands:
  ```shell
  nix build .#your-package-name:exe:your-exe-name
  ```
* With Nix legacy commands:
  ```shell
  nix-build -A your-package-name.components.exes.your-exe-name
  ```

Cross compilation is builtins into Flakes, but to cross-compile with legacy Nix commands, use the `projectCross` attribute:
```
nix-build -A projectCross.ghcjs.hsPkgs.your-package-name.components.exes.your-exe-name
nix-build -A projectCross.mingwW64.hsPkgs.your-package-name.components.exes.your-exe-name
```

Flakes provide a `devShell` attribute that allow you to spawn a developer shell, here with `cabal`, `hlint` and `haskell-language-server`:
```shell
nix develop .
cabal repl your-package-name:lib:your-package-name
cabal build your-package-name
```

Nix legacy commands needs you to add first a `shell.nix`:
```nix
{{#include getting-started/shell.nix}}
```
Then you can run:
```shell
nix-shell
cabal new-repl your-package-name:library:your-package-name
cabal new-build your-package-name
```

To open a shell for use with `stack` see [the following issue](https://github.com/input-output-hk/haskell.nix/issues/689#issuecomment-643832619).

## Getting started with Hix

Hix is a command line tool that provides an easy way to add `haskell.nix` support to existing haskell projects.

The `hix init` command adds a `flake.nix` and `nix/hix.nix` file. After that the project can be used with regular `nix` tools.

For instance to run `cabal build` on the `hello` package from hackage:
```shell
cabal unpack hello
cd hello-1.0.0.2
nix run "github:input-output-hk/haskell.nix#hix" -- init
nix develop
cabal build
```

To view the contents of the flake run:
```shell
nix flake show
```

To build a component with nix:
```shell
nix build .#hello:exe:hello
```

To build and run a component:
```shell
nix run .#hello:exe:hello
```

### Installing Hix

To use the other Hix features first install Hix with:
```shell
nix-env -iA hix -f https://github.com/input-output-hk/haskell.nix/tarball/master
```

To update run to the latest version run:
```shell
hix update
```

### Using `hix develop`, `hix flake`, `hix build` and `hix run`

These commands work the same as the `nix` versions without using the `flake.nix`.  Instead a boiler plate `haskell.nix` `flake.nix` file is added to `.hix-flake/flake.nix` and used from there.

The is can be useful if the project already includes a `flake.nix` or if you do not intend to maintain one.


Then all of these should work without the need to run `hix init`:
```shell
hix develop
hix flake show
hix build .#hello:exe:hello
hix run .#hello:exe:hello
```

### Using `hix-shell` and `hix-build`

These commands behave like `nix-build` and `hix-shell` would if a boiler plate `default.nix` and `shell.nix` we present.
```shell
hix-shell --run 'cabal build all'
hix-build -A hsPkgs.hello.components.exes.hello
```

## Going forward

Read through [project](../reference/library.md#project) function reference to see how the API works.

There are a number of things to explore further in the tutorials section.