Cross compilation of Haskell projects involves building a version of
GHC that outputs code for the target platform, and providing builds of
all library dependencies for that platform.

First, understand how to cross-compile a normal package from
Nixpkgs. Matthew Bauer's [Beginners' guide to cross compilation in
Nixpkgs][bauer] is a useful resource.

[bauer]: https://matthewbauer.us/blog/beginners-guide-to-cross.html 


Using an example from the guide, this builds GNU Hello for a Raspberry
Pi:

    nix build -f '<nixpkgs>' pkgsCross.raspberryPi.hello

We will use the same principle in [Haskell.nix][] — replacing the normal
package set `pkgs` with a cross-compiling package set
`pkgsCross.raspberryPi`.

### Raspberry Pi example

This is an example of using [Haskell.nix][] to build the [Bench][]
command-line utility, which is a Haskell program.

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  native = haskellNix { inherit pkgs; };
in
  native.haskellPackages.bench.components.exes.bench
```

Now switch the package set as in the previous example:

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  raspberryPi = haskellNix { pkgs = pkgs.pkgsCross.raspberryPi; };
in
  raspberryPi.haskellPackages.bench.components.exes.bench
```

You should be prepared for a long wait because it first needs to build
GHC, before building all the Haskell dependencies of [Bench][]. If all
of these dependencies compiled successfully, I would be very surprised!

!!! hint
    The above example won't build, but you can try and see, if you like.
    It will fail on [clock-0.7.2](http://hackage.haskell.org/package/clock-0.7.2),
    which needs a patch to build.

To fix the build problems, you must add extra configuration to the
package set. Your project will have a [`mkStackPkgSet`](../reference/library.md#mkstackpkgset) or
[`mkCabalProjectPkgSet`](../reference/library.md#mkcabalprojectpkgset). It is there where you must add
[module options](../reference/modules.md) for setting compiler flags, adding patches, and so on.


### Static executables with Musl libc

Another application of cross-compiling is to produce fully static
binaries for Linux. For information about how to do that with the
[Nixpkgs Haskell infrastructure][nixpkgs] (not [Haskell.nix][]), see
[nh2/static‑haskell‑nix][nh2]. Vaibhav Sagar's linked [blog
post][vaibhav] is also very informative.


```nix
{ pkgs ? import <nixpkgs> {} }:
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  musl64 = haskellNix { pkgs = pkgs.pkgsCross.musl64; };
in
  musl64.haskellPackages.bench.components.exes.bench
```

This example will build [Bench][] linked against Musl libc. However
the executable will still be dynamically linked. To get fully static
executables you must add package overrides to:

1. Disable dynamic linking
2. Provide static versions of system libraries. (For more details, see
   [Vaibhav's article][vaibhav]).

```nix
{
  packages.bench.components.exes.bench.configureFlags =
    stdenv.lib.optionals stdenv.hostPlatform.isMusl [
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-L${gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${zlib.static}/lib"
    ];
}
```

!!! note "Licensing"
    Note that if copyleft licensing your program is a problem for you,
    then you need to statically link with `integer-simple` rather than
    `integer-gmp`. However, at present, [Haskell.nix][] does not provide
    an option for this.


### How to cross-compile your project

Set up your project Haskell package set.

```
# default.nix
{ pkgs ? import <nixpkgs> {}
let
  # Import the Haskell.nix library,
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {
    inherit pkgs;
  };

  # Instantiate a package set using the generated file.
  pkgSet = haskell.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [
      {
        # You will need to put build fixes here.
      }
    ];
  };
in
  pkgSet.config.hsPkgs
```

Apply that package set to the Nixpkgs cross package sets that you are
interested in.

```
# release.nix
let
  project = import ./default.nix;
  pkgs = import <nixpkgs> {};
  native = import project { inherit pkgs; };
  crossRasperryPi = import project { pkgs = pkgs.pkgsCross.raspberryPi; };
in {
  my-project-native = native.my-project.components.exes.my-project;
  my-project-rasberry-pi = crossRaspberryPi.my-project.components.exes.my-project;
}
```

Try to build it, and apply fixes to the `modules` list, until there
are no errors left.



[nh2]: https://github.com/nh2/static-haskell-nix
[vaibhav]: https://vaibhavsagar.com/blog/2018/01/03/static-haskell-nix/
[haskell.nix]: https://github.com/input-output-hk/haskell.nix
[bench]: https://hackage.haskell.org/package/bench
[nixpkgs]: https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure
