# Manually generating Nix expressions

We believe that imports from derivations (IFDs) provide tremendous
value in nix and the aversion towards them stems mostly from
poor tooling and ci support for them.  We do not believe
that poor tooling or ci support should cripple nix capability
of abstraction.  Hence haskell.nix makes excessive use of
IFDs.

We do note however that there are users who prefer to
have IFD-free expressions.  For this group of users we
detail how to expand the IFD dependent high level functions
into their IFD free building blocks.

The general structure will be the same, independent of the use of
Stack or Cabal.

Let us assume for now that we have already generated a `pkgs.nix`
expression (see the links bellow). The following file then produces a package set:

```nix
{{#include manually-generating-nix-expressions/default.nix}}
```

With this setup you can then start building the components of
interest:

```shell
nix build -f default.nix $pkg.components.library
```

to build the library for `$pkg` or

```shell
nix build -f default.nix $pkg.components.exes.$exe
```

to build a specific executable. The same holds for test suites and benchmarks.

## Using Stack

With [nix-tools installed](./installing-nix-tools.md), we can simply run the
following command on a stack project:

```shell
stack-to-nix --output . --stack-yaml stack.yaml
```

This will produce a `pkgs.nix` file that looks like the following:
```nix
{
  resolver = "lts-12.17";
  extras = hackage:
    {
      packages = {
        "o-clock" = hackage.o-clock."0.1.1".revisions.default;
        ...
      } // {
        my-package = ./my-package.nix;
        ...
      };
    };
}
```

This file contains the stackage resolver, as well as set of extra
packages.  The extras specifies which `extra-deps` (here:
`o-clock-0.1.1`) we wanted to add over the stackage snapshot, and what
local packages we want (here: `my-package`).

## Using Cabal

### Generating `plan.json`

To get a plan, you need Cabal and GHC. See the [How to install a
compiler section of the Nixpkgs Manual][compiler] for information
about how to choose a specific compiler version.

[compiler]: https://nixos.org/nixpkgs/manual/#how-to-install-a-compiler

> **Note:** Cabal version
>
> The minimum Cabal version is 2.4. This version is available
> in the NixOS 19.03 release.

For this example, we will run a `nix-shell` with the default GHC
version for Nixpkgs.

```shell
nix-shell -p haskellPackages.cabal-install haskellPackages.ghc \
    --run "cabal new-configure"
```

If all goes well, you should now have the file
`dist-newstyle/cache/plan.json`.

> **Tip:** Specifying the GHC version
>
> To use a specific compiler version, replace `haskellPackages.ghc`
> with something like `haskell-nix.compiler.ghc865`. The given compiler
> must exist in your Nixpkgs version, of course. See also the
> [Nixpkgs Manual][compiler].

### Using `plan-to-nix`

With [nix-tools installed](./installing-nix-tools.md), we can then run the
following command on a Cabal project and its build plan. Omit the
`--cabal-project` option if you don't have a project file.

```shell
# convert the plan.json file into a pkgs.nix file
plan-to-nix --output . \
    --plan-json dist-newstyle/cache/plan.json
    --cabal-project cabal.project
```

This will produce a `pkgs.nix` file that looks like the following:

```nix
{
  pkgs = hackage:
    {
      packages = {
        "o-clock" = hackage.o-clock."0.1.1".revisions.default;
        ...
      };
      compiler = { ... };
    };

  extras = hackage:
    { packages = { my-package = ./.plan.nix/my-package.nix; }; };
}
```

It has converted Cabal's build plan into a Nix expression that selects
dependencies from `hackage.nix`. All local packages in the project are
generated with `cabal-to-nix` and added to the package set
description.
