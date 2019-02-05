# User Guide

So you want to use `haskell.nix` with your stack or cabal project. The
general appraoch will be to pick the right tool from `nix-tools` and
produce a `pkgs.nix` expressions.  Getting a copy of the haskell.nix,
hackage.nix (and potentially stackage.nix) source will then equip us
to produce derivations that we can `nix build`.

## Setup

The general structure will be the same for haskell.nix, independent of
the use of stack or cabal.  Let us assume for now that we have
generated a `pkgs.nix` expression in `nix`.

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

Next we will use this to import `haskell.nix`, `hackage.nix` and
`stackage.nix` (if we use a stack project).

**NOTE**: update the `rev` and `sha256` values to the recent ones as
  found on github.  Especially `hackage.nix` and `stackage.nix` will
  evolve with package release on hackage and stackage releases
  respectively.

```nix
let
  # all packages from hackage as nix expressions
  hackage = import (overrideWith "hackage"
                    (pkgs.fetchFromGitHub { owner  = "input-output-hk";
                                            repo   = "hackage.nix";
                                            rev    = "1bee48237cb87d9ad6432a9ddb7777f858674400";
                                            sha256 = "0sllxmffrcvcq63wlvz64s0i0ls2249vwix645899k6xh159z5pj";
                                            name   = "hackage-exprs-source"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "input-output-hk";
                                            repo   = "haskell.nix";
                                            rev    = "dae8025469d94e739aa52f23685bcd87840c72e3";
                                            sha256 = "0bj577aj2v7yl6d1l6rinp0bbn4k9m8k4jd46m4hspqaisfd58hg";
                                            name   = "haskell-lib-source"; }))
                   hackage;

  # the set of all stackage snapshots
  stackage = import (overrideWith "stackage"
                     (pkgs.fetchFromGitHub { owner  = "input-output-hk";
                                             repo   = "stackage.nix";
                                             rev    = "5ccfc7662469843768a5c4924d91faafbe5824e1";
                                             sha256 = "1zwasyscqn4751i10165imwj4715hh5arwmccqkpvpn9bnb6c5ck";
                                             name   = "stackage-snapshot-source"; }))
                   ;
in
```

Finally we string this together and produce a package set:

```nix
let
  my-pkgs = import ./nix/pkgs.nix;

  pkgSet = haskell.mkPkgSet {
    inherit pkgs;
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
