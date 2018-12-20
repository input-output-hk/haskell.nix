{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  # all packages from hackage as nix expressions
  hackage = import (fetchFromGitHub { owner  = "angerman";
                                      repo   = "hackage.nix";
                                      rev    = "66c28064da46525711722b75b4adb2ac878897d3";
                                      sha256 = "12ffzzjgirwzha3ngxbniccgn19406iryxspq19kgi4kz9lz6bpr";
                                      name   = "hackage-exprs-source"; });

  # The new Haskell infra applied to nix representation of Hackage
  haskell = import ../. hackage;

in {
  cabal-simple = callPackage ./cabal-simple { inherit haskell; };
  cabal-22 = callPackage ./cabal-22 { inherit haskell; };
}

## possible test cases
# 1. fully static linking
# 2. presence of haddock
# 3. cabal 2.4 stuff
# 4. multiple libraries in a cabal project
