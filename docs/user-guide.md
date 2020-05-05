# User Guide

[Haskell.nix][] can be use for two different use cases:

### Use 1 : to build a specific package from hackage or a [stackage][] lts or nightly set.

To build a package, say [lens][], from a stackage snapshot, say [lts-13.28][],
you could run
```bash
nix build '(with import <nixpkgs> (import ./. {}).nixpkgsArgs; haskell-nix.snapshots."lts-13.28").lens.components.library'
```
which would build the [lens][] library component from the lens package as fixed
by the [lts-13.28][] stackage snapshot.
   
To build any package from hackage, say [lens][], in version, say 4.17.1, you
could run
```bash
nix build '(with import <nixpkgs> (import ./. {}).nixpkgsArgs; (haskell-nix.hackage-package { name = "lens"; version = "4.17.1"; })).components.library'
```
which would build the [lens][] library component from the [lens-4.17.1][] package
from hackage.  The dependencies would be solved against the most recent 
[hackage-index-state][] that comes via the [hackage.nix][] pin with your
[haskell.nix][] checkout.  A specific one can be specified as well:
```bash
nix build '(with import <nixpkgs> (import ./. {}).nixpkgsArgs; (haskell-nix.hackage-package { name = "lens"; version = "4.17.1"; index-state = "2019-07-14T00:00:00Z"; })).components.library'
```
which would use the hackage index as of `2019-07-14T00:00:00Z` to produce a build plan
for the [lens-4.17.1][] package.
   

### Use 2 : to build a stack or cabal project.
 
So you want to use [Haskell.nix][] with your stack or cabal project. The
general approach will be to pick the right tool from `nix-tools` and
produce a `pkgs.nix` expression.  Getting a copy of the `nix-tools`
(and potentially the [Haskell.nix][] source), will then equip us to
produce derivations that we can `nix build`.

[haskell.nix]: https://github.com/input-output-hk/haskell.nix
[stackage]: https://stackage.org
[lts-13.28]: https://www.stackage.org/lts-13.28
[lens]: https://hackage.haskell.org/package/lens
[lens-4.17.1]: https://hackage.haskell.org/package/lens-4.17.1
[hackage.nix]: https://github.com/input-output-hk/hackage.nix
[hackage-index-state]: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix

## Installing `nix-tools`

To build the latest `nix-tools` and store the result at `./nt`, run:

```bash
nix build '(with import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}).nixpkgsArgs; haskell-nix.nix-tools)' --out-link nt
```

If you would like to then install `nix-tools` into your profile, run:

```bash
nix-env -i ./nt
```

## Getting the [Haskell.nix][] source (optional)

The [Haskell.nix][] and `nix-tools` source will be useful if you would
like to contribute improvements, or read the source code to fully
understand something that the documentation doesn't cover.

```bash
git clone https://github.com/input-output-hk/nix-tools
git clone https://github.com/input-output-hk/haskell.nix
cd haskell.nix
nix build -f . haskell-nix.nix-tools --out-link nt
```

## Setting up the Cachix binary cache

CI pushes to [cachix](https://cachix.org) so you can benefit from the cache if
you pin a combination of `haskell.nix` and `nixpkgs` built by CI. You'll need
to configure the [nix-tools cachix](https://nix-tools.cachix.org) as a
`substituter` for `nix` and add the public key found at the url to
`trusted-public-keys`.

## Using [Haskell.nix][] with your project

The easiest way to get a hold of [Haskell.nix][] is with
[`fetchTarball`](https://nixos.org/nix/manual/#ssec-builtins).

```nix
import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}).nixpkgsArgs
```

### Using your cabal.project file

If your project has a `cabal.project` you can add a `default.nix` like this:

```nix
{ pkgs ? import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}).nixpkgsArgs
, haskellCompiler ? "ghc865"
}:
  pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.buildPackages.pkgs.haskell-nix.compiler.${haskellCompiler};
    # pkg-def-extras = [
    #   # Additional packages ontop of all those listed in `cabal.project`
    # ];
    # modules = [{
    #   # Specific package overrides would go here for example:
    #   packages.cbors.package.ghcOptions = "-Werror";
    #   packages.cbors.patches = [ ./one.patch ];
    #   packages.cbors.flags.optimize-gmp = false;
    #   # It may be better to set flags in `cabal.project` instead
    #   # (`plan-to-nix` will include them as defaults).
    # }];
  }
```

Note: The `cleanGit` call will exclude any files not in the index file
in the local `.git` directory.  This is extremely useful as it avoids
unwanted builds (for instance when the contents of `dist-newstyle` are
modified by cabal).  It is important that `src` is the root directory
of the repo (so `cleanGit` can find the `.git` directory).  If the project
is not at the root of the repo, then also pass `subDir` to `cleanGit` with the
location of the project relative to `src` as a string ([more information
on using cleanGit](clean-git.md)). For example:

```
  src = pkgs.haskell-nix.haskellLib.cleanGit
    { src = ./.; subDir = "subdir/another-subdir"; };
```

If you want to use a custom filtering function, don't use the built-in nix `filterSource` function.
Due to some technicalities about how filtering and the nix store work, it will cause all components in
your project to be rebuilt any time any part of the project source changes. Use `cleanSourceWith` instead:

```
  src = pkgs.haskell-nix.haskellLib.cleanSourceWith
    { src = ./.; filter = myFilterFunction; };
```

Note that `cleanSourceWith` will also take the `subDir` argument.

You can build a component from your project with `nix-build` (in this
case the `hello` executable in a `helloworld` package):

```bash
nix-build -A helloworld.components.exes.hello
```

For interactive development you can run:

```bash
nix-shell -A shellFor
```

The resulting shell will include all the dependencies of the local packages
listed in your `cabal.project` file (but it will not build those local
packages).

Inside the shell use `cabal new-build` and `cabal new-repl` to work on the
cabal project as you normally would.  They will use the packages provided
by nix whenever possible.

It is a good idea to add `write-ghc-environment-files: never` to your
`cabal.project` file to prevent unwanted `.ghc.environment.*` files
(they will prevent subsequent `nix-shell` invocations from working
properly).

Adding an `index-state` to your `cabal.project` can pin the plan used so that
the same packages will be built each time (until you change the index-state
or one of the constraints in your local `.cabal` files).

### Using your stack.yaml file

If your project has a `stack.yaml` you can add a `default.nix` like this:

```nix
{ pkgs ? import <nixpkgs> (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}).nixpkgsArgs
}:
  pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    # pkg-def-extras = [
    #   # Additional packages ontop of all those listed in `stack.yaml`
    # ];
    # modules = [
    #   # Specific package overrides would go here for example:
    #   packages.cbors.package.ghcOptions = "-Werror";
    #   packages.cbors.patches = [ ./one.patch ];
    #   packages.cbors.flags.optimize-gmp = false;
    #   # It may be better to set flags in `stack.yaml` instead
    #   # (`stack-to-nix` will include them as defaults).
    # ];
  }
```

You can build a component from your project with `nix-build` (in this
case the `hello` executable in a `helloworld` package):

```bash
nix-build -A helloworld.components.exes.hello
```

### Pinning the [Haskell.nix][] version

For simplicity's sake we will use `fetchTarball` for the examples in
this documentation. This will always get the latest version, and is
similar to an auto-updating Nix channel.

However, in your own project, you may wish to pin [Haskell.nix][] (as
you would pin Nixpkgs). This will make your builds reproducable, more
predictable, and faster (because the fixed version is cached).

One way of doing this is to use `nix-prefetch-git` to get a JSON file
with a revision and SHA-256 hash of [Haskell.nix][].

```
$ nix-prefetch-git --quiet https://github.com/input-output-hk/haskell.nix | tee haskell-nix-src.json
{
  "url": "https://github.com/input-output-hk/haskell.nix",
  "rev": "f1a94a4c82a2ab999a67c3b84269da78d89f0075",
  "date": "2019-06-05T01:06:12+00:00",
  "sha256": "0ggxsppjlb6q6a83y12cwgrdnqnw1s128rpibgzs5p1966bdfqla",
  "fetchSubmodules": false
}
```

(The `tee` command is just to show you the result.)
Use the following expression to import that version:

```nix
{ nixpkgs ? <nixpkgs> }:

let
  spec = builtins.fromJSON (builtins.readFile ./haskell-nix-src.json);
  haskell-nix-src = (import nixpkgs {}).fetchgit {
    name = "haskell-lib";
    inherit (spec) url rev sha256 fetchSubmodules;
  };
in
  import nixpkgs (import haskell-nix-src)
```

There are other possible schemes for pinning. See
[`haskell.nix/lib/fetch-external.nix`](https://github.com/input-output-hk/haskell.nix/blob/master/lib/fetch-external.nix),
the [niv](https://github.com/nmattia/niv) tool, or the Nix Flakes
proposal.

### Overriding the Hackage version

Sometimes you might want to use a chosen version of Haskell.nix with a
recent update of Hackage or Stackage. This can be done with JSON pins:

```bash
nix-prefetch-git https://github.com/input-output-hk/hackage.nix | tee hackage-src.json
nix-prefetch-git https://github.com/input-output-hk/stackage.nix | tee stackage-src.json
```

The resulting JSON files will correspond to the latest revision of
[hackage.nix][] and [stackage.nix][]. See
[Architecture](architecture.md) for more information about how
these Git repositories correspond to the actual Hackage and Stackage.

[hackage.nix]: https://github.com/input-output-hk/hackage.nix
[stackage.nix]: https://github.com/input-output-hk/stackage.nix

```nix
{ pkgs ? import <nixpkgs> (haskellNixArgs // { overlays = haskellNixArgs.overlays ++ [
  (self: super: {
    haskell-nix = super.haskell-nix // {
      hackageSourceJSON  = ./hackage-src.json;
      stackageSourceJSON = ./stackage-src.json;
    };
  })]; })
, haskellNixArgs ? (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}).nixpkgsArgs
}:
  pkgs
```

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

Loading 'example2.nix'...
Added 2 variables.

nix-repl> lib.take 5 pkgNames
[ "ALUT" "ANum" "Allure" "Boolean" "BoundedChan" ]

nix-repl> 
```

Now that you have `nix-tools` and are able to import [Haskell.nix][],
you can continue to the next chapter.
