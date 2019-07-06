# How to create Nix builds for your own private Haskell packages

The general structure will be the same, independent of the use of
Stack or Cabal.

Let us assume for now that we have already generated a `pkgs.nix`
expression. The following file then produces a package set:

```nix
# default.nix
let
  # Import the Haskell.nix library,
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};

  # Import the file you will create in the stack-to-nix or cabal-to-nix step.
  my-pkgs = import ./pkgs.nix;

  # Stack projects use this:
  pkgSet = haskell.mkStackPkgSet {
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
      #  packages.cbors.patches = [ ./one.patch ];
      #  packages.cbors.flags.optimize-gmp = false;
    ];
  };

  # Cabal projects use this:
  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = my-pkgs;
    pkg-def-extras = [];
    modules = [];
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
