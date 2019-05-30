{ system ? builtins.currentSystem
, pkgs ? import nixpkgs { inherit system; }
, nixpkgs ? ../nixpkgs
, haskell ? pkgs.callPackage ../. { }
}:

with pkgs;

let
  util = callPackage ./util.nix {};

in {
  cabal-simple = haskell.callPackage ./cabal-simple { inherit util; };
  cabal-sublib = haskell.callPackage ./cabal-sublib { inherit util; };
  cabal-22 = haskell.callPackage ./cabal-22 {};
  with-packages = haskell.callPackage ./with-packages { inherit util; };
  builder-haddock = haskell.callPackage ./builder-haddock {};
  stack-simple = haskell.callPackage ./stack-simple {};
  snapshots = haskell.callPackage ./snapshots {};
  shell-for = haskell.callPackage ./shell-for {};
  callStackToNix = haskell.callPackage ./callStackToNix {};
  callCabalProjectToNix = haskell.callPackage ./call-cabal-project-to-nix {};
  cabal-source-repo = haskell.callPackage ./cabal-source-repo {};

  # Run unit tests with: nix-instantiate --eval --strict -A unit
  # An empty list means success.
  unit = haskell.callPackage ./unit.nix {};
}

## more possible test cases
# 1. fully static linking
# 2. cabal 2.4 stuff
# 3. cross-compiling
