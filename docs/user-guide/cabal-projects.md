Here we will look into how to generate the `pkgs.nix` file for a
`cabal.project` project. For the full integration see the [previous
page](./projects.md).

Generating package sets for Cabal projects, is slightly more involved,
because you need to let Cabal calculate a build plan for the
dependencies of your package(s).

## Generating `plan.json`

To get a plan, you need Cabal and GHC. See the [Nixpkgs
Manual](https://nixos.org/nixpkgs/manual/#how-to-install-a-compiler)
for information about how to choose a specific compiler version.

For this example, we will run a `nix-shell` with the default GHC
version for Nixpkgs.

```bash
nix-shell -p haskellPackages.cabal-install haskellPackages.ghc \
    --run "cabal new-configure"
```

If all goes well, you should now have the file
`dist-newstyle/cache/plan.json`.

## Using `plan-to-nix`

With [nix-tools installed](../user-guide.md), we can then run the
following command on a Cabal project and its build plan. Omit the
`--cabal-project` option if you don't have a project file.

```bash
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

## Creating the package set

Import the generated `pkgs.nix` and pass to
[`mkCabalPkgSet`](../reference/library.md#mkcabalprojectpkgset) to
instantiate a package set.

```nix
# default.nix
let
  # Import the Haskell.nix library,
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};

  # Instantiate a package set using the generated file.
  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };
in
  pkgSet.config.hsPkgs
```
