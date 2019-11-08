# How to replace IFDs with intermediate nix files

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
# default.nix
let
  # Import the Haskell.nix library,
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  pkgs = haskellNix.nixpkgs {};

  # Import the file you will create in the stack-to-nix or cabal-to-nix step.
  my-pkgs = import ./pkgs.nix;

  # Stack projects use this:
  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = my-pkgs;
    pkg-def-extras = [
      # these extras will provide additional packages
      # ontop of the package set.  E.g. extra-deps
      # for stack packages. or local packages for
      # cabal.projects
    ];
    modules = [
      # specific package overrides would go here
      # example:
      #  packages.cbors.package.ghcOptions = "-Werror";
      #  packages.cbors.patches = [ ./one.patch ];
      #  packages.cbors.flags.optimize-gmp = false;
      # It may be better to set flags in stack.yaml instead
      # (`stack-to-nix` will include them as defaults).
    ];
  };

  # Cabal projects use this:
  pkgSet = pkgs.haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = my-pkgs;
    pkg-def-extras = [];
    modules = [
      # specific package overrides would go here
      # example:
      #  packages.cbors.package.ghcOptions = "-Werror";
      #  packages.cbors.patches = [ ./one.patch ];
      #  packages.cbors.flags.optimize-gmp = false;
      # It may be better to set flags in `cabal.project` instead
      # (`plan-to-nix` will include them as defaults).
    ];
  };

in pkgSet.config.hsPkgs // { _config = pkgSet.config; }
```

With this setup you can then start building the components of
interest:

```bash
nix build -f default.nix $pkg.components.library
```

to build the library for `$pkg` or

```bash
nix build -f default.nix $pkg.components.exes.$exe
```

to build a specific executable. The same holds for test suites and benchmarks.

- [Generating a `pkgs.nix` for a Stack project](./stack-projects.md)
- [Generating a `pkgs.nix` for a Cabal project](./cabal-projects.md)
