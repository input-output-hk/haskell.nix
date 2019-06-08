# User Guide

So you want to use [Haskell.nix][] with your stack or cabal project. The
general approach will be to pick the right tool from `nix-tools` and
produce a `pkgs.nix` expression.  Getting a copy of the `nix-tools`
(and potentially the [Haskell.nix][] source), will then equip us to
produce derivations that we can `nix build`.

[haskell.nix]: https://github.com/input-output-hk/haskell.nix

## Installing `nix-tools`

To build the latest `nix-tools` and store the result at `./nt`, run:

```bash
nix build -f https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz nix-tools --out-link nt
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
nix build -f . nix-tools --out-link nt
```

## Importing [Haskell.nix][] into your project

The easiest way to get a hold of [Haskell.nix][] is with
[`fetchTarball`](https://nixos.org/nix/manual/#ssec-builtins).

```nix
import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}
```

If you have your own `pkgs` variable, pass it to [Haskell.nix][] to
prevent double-evaluation of the Nixpkgs collection.

```nix
{ pkgs ? import <nixpkgs> {} }:

import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) { inherit pkgs; }
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
{ pkgs ? import <nixpkgs> {} }:

let
  spec = builtins.fromJSON (builtins.readFile ./haskell-nix-src.json);
  haskell-nix-src = pkgs.fetchgit {
    name = "haskell-lib";
    inherit (spec) url rev sha256 fetchSubmodules;
  };
in
  import haskell-nix-src { inherit pkgs; }
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
{ pkgs ? import <nixpkgs> {} }:

import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {
  inherit pkgs;
  hackageSourceJSON = ./hackage-src.json;
  stackageSourceJSON = ./stackage-src.json; 
}
```

## Using `nix repl`

It's sometimes useful to load [Haskell.nix][] in the REPL to explore
attrsets and try examples. 

```
# example.nix
{ pkgs ? import <nixpkgs> {} }:
rec {
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) { inherit pkgs; };
  pkgNames = pkgs.lib.attrNames haskell.snapshots."lts-13.18";
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
