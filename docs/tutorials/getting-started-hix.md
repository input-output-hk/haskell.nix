# Getting started with Hix

The `hix` tools are wrappers for the various `nix` tools that
use `haskell.nix` without the need to add any `.nix` files.

This is useful for:

* A quick way to try out haskell.nix for new users.

* Using haskell.nix to work on projects that do not have
  `.nix` files.

* Testing to see if `haskell.nix` can build a project.

* Making `flake` and `non flake` configurations to check `haskell.nix`
  treats them the same.

## Installing Nix

To use Hix you will need to install [Nix](https://nixos.org/download.html).

## Setting up the binary cache

IMPORTANT: you *must* do this or you *will* build several copies of GHC!

You can configure Nix to use our binary cache, which is pushed to by CI, so should contain the artifacts that you need.

You need to add the following sections to `/etc/nix/nix.conf` or, if you are a trusted user, `~/.config/nix/nix.conf` (if you don't know what a "trusted user" is, you probably want to do the former).

```
trusted-public-keys = [...] hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= [...]
substituters = [...] https://hydra.iohk.io [...]
```

This can be tricky to get setup properly. If you're still having trouble getting cache hits, consult the corresponding [troubleshooting section](../reference/troubleshooting#why-am-i-building-ghc).

## Installing Hix

```
nix-env -iA hix -f https://github.com/input-output-hk/haskell.nix/tarball/master
```

## Updating Hix (also updates hackage)

```
hix update
```

This is also necessary to make the latest nightly snapshot of hackage
avaiable to nix.

## Building with Hix

To run `cabal build` in a nix-shell with all the dependencies required:

```
cabal unpack hello
cd hello-1.0.0.2
hix-shell --run 'cabal build'
```

Build with nix:

```
hix-build -A hsPkgs.hello.components.exes.hello
```

Cross compile to JavaScript:

```
hix-build -A projectCross.ghcjs.hsPkgs.hello.components.exes.hello
```

## Configuring Hix

The configuration arguments for `Hix` can be (from highest precedence to lowest):

* Passed on the command line with `--arg` (or `--argstr` for string args).

* Placed in `nix/hix.nix` file in the project dir.

* Placed in `~/.config/hix/hix.conf`

For example to build with GHC 8.10.7:

```
hix-shell --argstr compiler-nix-name ghc8107 --run 'cabal build'
```

or add a `nix/hix.nix` or `~/.config/hix/hix.conf` file:

```nix
{ compiler-nix-name = "ghc8107"; }
```

Here are just a few of the other configuration arguments you could use
in the files or on the command line (they are all optional):

```nix
{ name = "hello";                    # for better error messages and derivation names
  nixpkgsPin = "nixpkgs-unstable";   # or nixpkgs-2111 or nixpkgs-2105
  nixpkgs = <nixpkgs>;               # use this instead of nixpkgsPin
  subDir = "some/sub/dir";           # sub dir containing the haskell project
  projectFileName = "stack.yaml";    # use this project file
  tools.haskell-language-server = "latest";
  tools.hlint = "latest";            # Include the latest hls and hlint in the shell
  index-state = "2021-02-22T00:00:00Z"; # It is normally best to put this in `cabal.project` (not here)

# PLUS MANY MORE!  Almost any argument you can pass to the project functions
# or to `shellFor` can be used in as a Hix configuration argument.

}
```

## Adding Nix Support with Niv

If you have a `nix/hix.nix` file with suitable configuration that
you want to make available to users with Nix (without having to
install Hix).

[Niv](https://github.com/nmattia/niv) is a command line tool for keeping track of Nix project dependencies.

After installing niv you can initialize niv and pin the latest haskell.nix
commit by running the following in the root directory of the project:

```
niv init
niv add input-output-hk/haskell.nix -n haskellNix
```

Add `default.nix`:

```nix
(import (import nix/sources.nix).haskellNix {}).hix.project { src = ./.; }
```

If you want to also pin `nixpkgs` with Niv use:

```nix
let
  sources = import nix/sources.nix;
in
  (import sources.haskellNix {}).hix.project {
    inherit (sources) nixpkgs;
    src = ./.;
  }
```

Add `shell.nix`:

```nix
(import ./.).shell
```

When you want to update to the latest version of haskell.nix use:

```
niv update haskellNix
```

## Adding Nix Flake Support

To add flake support that uses the `nix/hix.nix` configuration in your
follow the [Getting started with flakes](getting-started.md) guide, but
use `haskell-nix.hix.project` instead of `haskell-nix.project'`

The `nixpkgs` used will need to be selected as a flake input (any selection
made in `nix/hix.nix` will be ignored).

Example `flake.nix` file:

```nix
{
  description = "A very basic flake";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
    let
      overlays = [ haskellNix.overlay
        (final: prev: {
          # This overlay adds our project to pkgs
          helloProject =
            final.haskell-nix.hix.project {
              src = ./.;
              # Other project options can be put in `nix/hix.nix`
            };
        })
      ];
      pkgs = import nixpkgs { inherit system overlays; inherit (haskellNix) config; };
      flake = pkgs.helloProject.flake {
        # This adds support for `nix build .#js-unknown-ghcjs:hello:exe:hello`
        crossPlatforms = p: [p.ghcjs];
      };
    in flake // {
      # Built by `nix build .`
      defaultPackage = flake.packages."hello:exe:hello";
    });
}
```


