# User Guide

So you want to use `haskell.nix` with your stack or cabal project. The
general	approach will be to pick the right tool from `nix-tools` and
produce a `pkgs.nix` expressions.  Getting a copy of the haskell.nix,
hackage.nix (and potentially stackage.nix) source will then equip us
to produce derivations that we can `nix build`.

## Setup

The general structure will be the same for haskell.nix, independent of
the use of stack or cabal.  Let us assume for now that we have
generated a `pkgs.nix` expression in `nix`.

- [Generating a `pkgs.nix` for a stack project](/user-guide-stack)
- [Generating a `pkgs.nix` for a cabal project](/user-guide-cabal)

## default.nix

We will start with defining a helper function in our `default.nix`
that will allow us to override the source imports with `-I
haskell=/path/to/haskell.nix` in case we need it.

```nix
{ pkgs ? import <nixpkgs> {}
}:
let
  overrideWith = override: default:
   let
     try = builtins.tryEval (builtins.findFile builtins.nixPath override);
   in if try.success then
     builtins.trace "using search host <${override}>" try.value
   else
     default;
in
```

Next we will use this to import `haskell.nix`.

**NOTE**: update the `rev` and `sha256` values to the recent ones as
  found on GitHub.  Especially `haskell.hackage` and `haskell.stackage`
  will evolve with package release on hackage and stackage releases
  respectively.

```nix
let
  haskellLib = pkgs.fetchFromGitHub {
    owner  = "input-output-hk";
    repo   = "haskell.nix";
    rev    = "5180ae9d78756509c81b98b8e6d974e350b15752";
    sha256 = "0fbnnvymdp2qb09wlqy6ga8wsyhglx607cjdfg510s1gs756v9yx";
    name   = "haskell-lib-source";
  };
  haskell = import (overrideWith "haskell" haskellLib) { inherit pkgs; };
in
```

Finally we string this together and produce a package set:

```nix
let
  # Import the file you will create in the stack-to-nix or cabal-to-nix step.
  my-pkgs = import ./nix/pkgs.nix;

  # Stack projects use the mkStackPkgSet helper function
  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = my-pkgs;
    pkg-def-overlays = [];
    modules = [];
  };

  # Cabal projects use mkPkgSet
  pkgSet = haskell.mkPkgSet {
    pkg-def = my-pkgs.pkg-def;
    pkg-def-overlays = [
      # this overlay will provide additional packages
      # ontop of the package set.  E.g. extra-deps
      # for stack packages. or local packages for
      # cabal.projects
      my-pkgs.overlay
    ];
    modules = [
      # specific package overrides would go here
      # example:
      #  packages.cbors.patches = [ ./one.patch ];
      #  packages.cbors.flags.optimize-gmp = false;
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
