{ pkgs ? import nixpkgs { }
, nixpkgs ? ../nixpkgs
, haskell ? pkgs.callPackage ../. { }
}:

with pkgs;

let
  util = callPackage ./util.nix {};

in pkgs.recurseIntoAttrs {
  cabal-simple = haskell.callPackage ./cabal-simple { inherit util; };
  cabal-simple-prof = haskell.callPackage ./cabal-simple-prof { inherit util; };
  cabal-sublib = haskell.callPackage ./cabal-sublib { inherit util; };
  cabal-22 = haskell.callPackage ./cabal-22 { inherit util; };
  with-packages = haskell.callPackage ./with-packages { inherit util; };
  builder-haddock = haskell.callPackage ./builder-haddock {};
  stack-simple = haskell.callPackage ./stack-simple {};
  snapshots = haskell.callPackage ./snapshots {};
  shell-for = haskell.callPackage ./shell-for {};
  callStackToNix = haskell.callPackage ./call-stack-to-nix {};
  callCabalProjectToNix = haskell.callPackage ./call-cabal-project-to-nix {};
  cabal-source-repo = haskell.callPackage ./cabal-source-repo {};
  buildable = haskell.callPackage ./buildable {};
  project-flags-cabal = haskell.callPackage ./project-flags/cabal.nix {};
  project-flags-stack = haskell.callPackage ./project-flags/stack.nix {};
  ghc-options-cabal = haskell.callPackage ./ghc-options/cabal.nix {};
  ghc-options-stack = haskell.callPackage ./ghc-options/stack.nix {};

  # Run unit tests with: nix-instantiate --eval --strict -A unit.tests
  # An empty list means success.
  unit = let
    tests = haskell.callPackage ./unit.nix {};
  in runCommand "unit-tests" { passthru = { inherit tests; }; }
     (lib.concatMapStringsSep "\n" (t: "echo ${t.name} failed") tests +
      (if builtins.length tests == 0 then "\ntouch $out" else "\nexit 1"));
}

## more possible test cases
# 1. fully static linking
# 2. cabal 2.4 stuff
# 3. cross-compiling
