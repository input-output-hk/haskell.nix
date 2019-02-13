{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let

  # The new Haskell infra applied to nix representation of Hackage
  haskell = pkgs.callPackage ../. { };

  util = callPackage ./util.nix {};

in {
  cabal-simple = callPackage ./cabal-simple { inherit (haskell) mkPkgSet; inherit util; };
  cabal-22 = callPackage ./cabal-22 { inherit (haskell) mkPkgSet; };
  with-packages = callPackage ./with-packages { inherit (haskell) mkPkgSet; inherit util; };
  builder-haddock = callPackage ./builder-haddock { inherit (haskell) mkPkgSet; };
  stack-simple = callPackage ./stack-simple { inherit (haskell) mkStackPkgSet; };
  ifd = callPackage ./ifd { inherit (haskell) callCabalToNix; inherit (pkgs.haskellPackages) ghc; inherit util; };

  # Run unit tests with: nix-instantiate --eval --strict -A unit
  # An empty list means success.
  unit = callPackage ./unit.nix { inherit (haskell) haskellLib; };
}

## possible test cases
# 1. fully static linking
# 2. presence of haddock
# 3. cabal 2.4 stuff
# 4. multiple libraries in a cabal project
