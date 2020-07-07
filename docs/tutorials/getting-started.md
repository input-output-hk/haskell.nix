# Getting started

`haskell.nix` can automatically translate your
[Cabal](https://cabal.readthedocs.io/en/latest/cabal-project.html)
or [Stack](https://docs.haskellstack.org/en/stable/README/#quick-start-guide)
project and its dependencies into Nix code.

Assuming you have [Nix](https://nixos.org/download.html) installed, you can
start setting up your project.

## Setting up the Cachix binary cache

You can **avoid compiling GHC and nix-tools** by configuring
[Cachix](https://cachix.org) so you can benefit from the binary cache built by
CI:

```bash
$ nix-env -iA cachix -f https://cachix.org/api/v1/install
installing 'cachix-0.3.8'
building '/nix/store/bh176xhpk4wrjm56iahm86wf85jaz23v-user-environment.drv'...
created 42 symlinks in user environment

$ cachix use iohk
Configured https://iohk.cachix.org binary cache in ~/.config/nix/nix.conf
```

## Scaffolding

The following work with `stack.yaml` and `cabal.project` based
projects.

Add `default.nix`:

```nix
{ # Fetch the latest haskell.nix and import its default.nix
  haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2003

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
, nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
, pkgs ? import nixpkgsSrc nixpkgsArgs
}: pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "haskell-nix-project";
    src = ./.;
  };
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

### Pinning the [haskell.nix][] version

For simplicity's sake we will use `fetchTarball` for the examples in
this documentation. This will always get the latest version, and is
similar to an auto-updating Nix channel.

However, in your own project, you may wish to pin [haskell.nix][] (as
you would pin Nixpkgs). This will make your builds reproducible, more
predictable, and faster (because the fixed version is cached).

Straightforward way of doing this is to change the branch name to a revision.

```nix
{ # Fetch a specific haskell.nix and import its default.nix
 haskellNix ? import (builtins.fetchTarball "https://github.com/input-output-hk/haskell.nix/archive/f1a94a4c82a2ab999a67c3b84269da78d89f0075.tar.gz") {}

...
```

There are other possible schemes for pinning. See
[Bumping Hackage and Stackage snapshots](./hackage-stackage.md) and
[Nix tutorial on reproducibility using pinning](https://nix.dev/tutorials/towards-reproducibility-pinning-nixpkgs.html).

## Going forward

Read through [project](../reference/library.md#project) function reference to see how the API works.

There are a number of things to explore further in the tutorials section.

[haskell.nix]: https://github.com/input-output-hk/haskell.nix
