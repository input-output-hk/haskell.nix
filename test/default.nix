{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let
  # all packages from hackage as nix expressions
  hackage = import (fetchFromGitHub { owner  = "angerman";
                                      repo   = "hackage.nix";
                                      rev    = "5223a45e08b1b0d738fdd292b39e49f39f21536f";
                                      sha256 = "09r662kn2qs444fmqni9jamaxnrk9jrg6whqmxbwhfgd5vy3yynq";
                                      name   = "hackage-exprs-source"; });

  # The new Haskell infra applied to nix representation of Hackage
  haskell = import ../. hackage;

  haskellLib = let hl = import ../lib { inherit lib; haskellLib = hl; }; in hl;

in {
  cabal-simple = callPackage ./cabal-simple { inherit haskell; };
  cabal-22 = callPackage ./cabal-22 { inherit haskell; };
  with-packages = callPackage ./with-packages { inherit haskell; };

  # Run unit tests with: nix-instantiate --eval --strict -A unit
  # An empty list means success.
  unit = callPackage ./unit.nix { inherit haskellLib; };
}

## possible test cases
# 1. fully static linking
# 2. presence of haddock
# 3. cabal 2.4 stuff
# 4. multiple libraries in a cabal project
