Haskell.nix also provides reproducible development environments for
your Haskell projects. These environments can contain not only GHC and
your Haskell package dependencies, but also the required system
libraries and build tools.

Inside the development shell, you can run commands such as `ghc`,
`ghci`, or `cabal new‑build` (`cabal build` on Cabal 3.0),
and they will have all dependencies available.

Every dependency will be cached in your Nix store. If you have set up
Hydra CI, then your team can share pre-built dependencies.

These examples assume that you have created your package set as
described in [Creating Nix builds for your projects](getting-started.md) and
it exists in a file called `default.nix`.

!!! note

    Old-style `cabal build` and `stack` builds are not (yet)
    supported. For example, `stack` will (by design) download and
    rebuild all dependencies, even though they are available in the
    shell. However, if you have a Stack project, you can generate the
    package set with Haskell.nix, then use `cabal new‑build` to work
    on it.  Starting Cabal 3.0 `cabal build` will work out of the box, as
    new style builds are the default.

## How to get a development shell

If you have a Cabal or Stack project with one or more packages
(i.e. multiple `.cabal` files, not a single package with multiple
components), then you will need a development environment that
contains the _dependencies_ of your packages, but not the packages
themselves. This is what the [`shellFor`][shellFor] function does.

```nix
# shell.nix
let
  project = import ./default.nix;
in
  project.shellFor {
    # ALL of these arguments are optional.

    # List of packages from the project you want to work on in
    # the shell (default is all the projects local packages).
    packages = ps: with ps; [
      pkga
      pkgb
    ];

    # Builds a Hoogle documentation index of all dependencies,
    # and provides a "hoogle" command to search the index.
    withHoogle = true;

    # Some common tools can be added with the `tools` argument
    tools = {
      cabal = "3.2.0.0";
      hlint = "latest"; # Selects the latest version in the hackage.nix snapshot
      haskell-language-server = "latest";
    };
    # See overlays/tools.nix for more details

    # Some you may need to get some other way.
    buildInputs = [ (import <nixpkgs> {}).git ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;
  }
```

See also: [Haskell.nix Library Reference: `shellFor`][shellFor]

[shellFor]: ../reference/library.md#shellfor

## How to get a local Hoogle index

If you need a local Hoogle for all the dependencies of your project create this file

```nix
# shell-hoogle.nix
let
  project = import ./default.nix {};
in
  project.shellFor {
      packages = ps: [ps.my-package];
      withHoogle = true;
  }
```

and run `nix-shell shell-hoogle.nix --run "hoogle server --local"`.
This will open a local Hoogle server at `http://127.0.0.1:8080`.


## How to get an ad-hoc development shell including certain packages

This creates a development environment with the given packages
registered in the package database. The `ghcWithPackages` function
operates on a Haskell.nix package set, and accepts an argument that
selects packages from the larger package set.

```nix
# shell.nix
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  nixpkgs = import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs;
  haskell = nixpkgs.haskell-nix;
in
  haskell.haskellPackages.ghcWithPackages (ps: with ps;
    [ lens conduit conduit-extra ])
```

If you need a Hoogle documentation index, use `ghcWithHoogle` in place
of `ghcWithPackages`.

## How to get packages from a certain Stackage snapshot

Haskell.nix knows about every released Stackage snapshot. You can use
it to build packages from a given snapshot, without setting up a full
project.

```nix
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {};
  nixpkgs = import haskellNix.sources.nixpkgs haskellNix.nixpkgsArgs;
  haskell = nixpkgs.haskell-nix;
in
  haskell.snapshots."lts-13.18".alex.components.exes.alex
```

There are Haskell.nix package sets for every Stackage snaphot under
`haskell.snapshots`.

The alias `haskell.haskellPackages` corresponds to the package set for
a recent LTS Haskell version.

You can use `ghcWithPackages` on any of these package sets to quickly
get a shell with some packages.

!!! warning
    The build will not work if your Nixpkgs does not contain the version
    of GHC specified in the snapshot. Nixpkgs only carries the
    latest version of each recent release series, so many snapshots
    can't be built.


## Emacs IDE support

Once you have a development shell, then you can begin configuring
Emacs to use it. The way I do it is:

1. Run [lorri watch](https://github.com/target/lorri) to continuously
   build the shell environment and maintain GC roots.

2. Use [emacs‑direnv](https://github.com/wbolster/emacs-direnv) to
   push the development environment into Emacs.

3. Use [Dante](https://github.com/jyp/dante) for highlighting errors
   and auto-completion. You must customize Dante to prevent it from
   automatically using `nix‑shell` or `stack`. Trim `dante‑methods` to
   just `new‑build` and `bare‑ghci`.

    You can also use [`.dir‑locals.el`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Directory-Variables.html)
    for this. If your project has multiple targets, set `dante‑target`
    per-directory.

4. For [`haskell‑mode`](https://github.com/haskell/haskell-mode)
   interactive Haskell, set `haskell‑process‑type` to
   `cabal‑new‑repl`.

## Using `nix repl`

It's sometimes useful to load [Haskell.nix][] in the REPL to explore
attrsets and try examples.

```
# example.nix
{ nixpkgs ? <nixpkgs> }:
rec {
  haskell = import nixpkgs (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}).nixpkgsArgs;
  pkgNames = haskell.pkgs.lib.attrNames haskell.haskell-nix.snapshots."lts-13.18";
}
```

Load the example file:

```
$ nix repl '<nixpkgs>' example.nix
Welcome to Nix version 2.3pre6631_e58a7144. Type :? for help.

Loading '<nixpkgs>'...
Added 10403 variables.

Loading 'example.nix'...
Added 2 variables.

nix-repl> lib.take 5 pkgNames
[ "ALUT" "ANum" "Allure" "Boolean" "BoundedChan" ]

nix-repl>
```

Now that you have `nix-tools` and are able to import [Haskell.nix][],
you can continue to the next chapter.

[haskell.nix]: https://github.com/input-output-hk/haskell.nix
