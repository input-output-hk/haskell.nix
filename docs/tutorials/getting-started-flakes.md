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

## Setting up the binary cache

IMPORTANT: you *must* do this or you *will* build several copies of GHC!

You can configure Nix to use our binary cache, which is pushed to by CI, so should contain the artifacts that you need.

You need to add the following sections to `/etc/nix/nix.conf` or, if you are a trusted user, `~/.config/nix/nix.conf` (if you don't know what a "trusted user" is, you probably want to do the former).

```
trusted-public-keys = [...] hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= [...]
substituters = [...] https://hydra.iohk.io [...]
```

This can be tricky to get setup properly. If you're still having trouble getting cache hits, consult the corresponding [troubleshooting section](../reference/troubleshooting#why-am-i-building-ghc).

## Scaffolding

The following work with `stack.yaml` and `cabal.project` based
projects.

Add `flake.nix`:

```nix
{
  description = "A very basic flake";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs?rev=07e5844fdf6fe99f41229d7392ce81cfe191bcfc";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, haskellNix, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        pkgs = haskellNix.legacyPackages.${system};
        project = pkgs.haskell-nix.project' {
          src = ./.;
          compiler-nix-name = "ghc884";
        };
     in project.flake {} // {
       # Built by `nix build .`
       defaultPackage = project.hsPkgs.hello.components.exes.hello;
       
       # This is used by `nix develop .` to open a shell for use with
       # `cabal`, `hlint` and `haskell-language-server`
       devShell = project.shellFor {
         tools = {
           cabal = "3.2.0.0";
           hlint = "latest";
           haskell-language-server = "latest";
         };
       };
     }
   );
}
```

!!! note "git dependencies"
    If you have git dependencies in your project, you'll need
    to [calculate sha256 hashes for them](./source-repository-hashes.md).

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
