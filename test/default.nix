let
  haskellNix = (import ../default.nix {});
in
{ pkgs ? import nixpkgs nixpkgsArgs
, nixpkgs ? haskellNix.sources.nixpkgs-default
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, ifdLevel ? 1000
}:

with pkgs;

let
  # Set recurseForDerivations for both children and grand-children values in
  # the input association list, but only if the key is "ifdInputs".
  #
  # withIfdInputs :: AttrSet -> AttrSet
  #
  # The values in the input attribute set must be attribute sets themselves.
  #
  # >>> withIfdInputs { ifdInputs = { plan-nix = { a = true; b = "hello"; }; }; cat = 2; }
  # { ifdInputs = {
  #     plan-nix = {
  #       a = true;
  #       b = "hello";
  #       recurseForDerivations = true;
  #     };
  #     recurseForDerivations = true;
  #   };
  #   cat = 2;
  # }
  #
  # >>> withIfdInputs { dog = "hello"; }
  # { dog = "hello"; }
  #
  # >>> withIfdInputs { }
  # { }
  withIfdInputs =
    builtins.mapAttrs
      (name: val:
        if name == "ifdInputs"
        then
          pkgs.recurseIntoAttrs
            (builtins.mapAttrs (_: v: pkgs.haskell-nix.withInputs v) val)
        else val
      );

  util = import ./util.nix { inherit (pkgs.haskell-nix) cabal-install; };

  # Map the values in an association list over the withIfdInputs function.
  #
  # addIfdInputsToVal :: AttrSet -> AttrSet
  #
  # The values in the input association list must be attribute sets themselves.
  addIfdInputsToVal = builtins.mapAttrs (_: val: withIfdInputs val);

  # Keep only the attribute with the key "ifdInputs".
  #
  # filterAttrsIfdInputs :: AttrSet -> AttrSet
  #
  # >>> filterAttrsIfdInputs { ifdInputs = 1; foobar = 2 }
  # { ifdInputs = 1 }
  #
  # >>> filterAttrsIfdInputs { foobar = "hello" }
  # { }
  filterAttrsIfdInputs = pkgs.lib.filterAttrs (n: _: n == "ifdInputs");

  # Remove all keys and values in a attribute set where the key
  # doesn't equal "ifdInputs".  Set the "recurseForDerivations"
  # key in the resulting value.
  #
  # filterNonIfdInputsSetRecurse :: AttrSet -> AttrSet
  #
  # >>> filterNonIfdInputsSetRecurse { ifdInputs = 1; foobar = 2 }
  # { ifdInputs = 1; recurseForDerivations = true }
  #
  # >>> filterNonIfdInputsSetRecurse { foobar = "hello" }
  # { recurseForDerivations = true; }
  filterNonIfdInputsSetRecurse = attrs:
    pkgs.recurseIntoAttrs (filterAttrsIfdInputs attrs);

  # Filter all out all the keys/values for child values of this attribute set
  # where the key is not equal to "ifdInputs".
  #
  # filterNonIfdInputsValues :: AttrSet -> AttrSet
  #
  # The values in the input AttrSet must be attribute sets themselves.
  #
  # >>> filterNonIfdInputsValues { foo = { ifdInputs = 1; cat = 2; }; bar = { dog = "hello"; }; }
  # { foo = {
  #     ifdInputs = 1;
  #     recurseForDerivations = true;
  #   };
  #   bar = {
  #     recurseForDerivations = true;
  #   };
  # }
  #
  # >>> filterNonIfdInputsValues { }
  # { }
  filterNonIfdInputsValues = attrs:
    builtins.mapAttrs (_: d: filterNonIfdInputsSetRecurse d) attrs;

  # Call filterNonIfdInputsValues on the input attribute set, but only
  # if ifdLevel is less than 3.  Otherwise, just return the attribute set.
  #
  # filterNonIfdInputsValuesLTLevel3 :: AttrSet -> AttrSet
  #
  # >>> filterNonIfdInputsValuesLTLevel3 2 { cabal-doctests = { ifdInputs = {...}; run = ""; }; cabal-simple = { run = ""; }; }
  # { cabal-doctests = {
  #     ifdInputs = {...};
  #     recurseForDerivations = true;
  #   };
  #   cabal-simple = {
  #     recurseForDerivations = true;
  #   };
  # }
  #
  # >>> filterNonIfdInputsValuesLTLevel3 1000 { cabal-doctests = { ifdInputs = {...}; run = "..."; }; cabal-simple = { run = "..."; }; }
  # { cabal-doctests = {
  #     ifdInputs = {...};
  #     run = "...";
  #     recurseForDerivations = true;
  #   };
  #   cabal-simple = {
  #     run = "...";
  #     recurseForDerivations = true;
  #   };
  # }
  #
  # >>> filterNonIfdInputsValuesLTLevel3 0 { }
  # { }
  filterNonIfdInputsValuesLTLevel3 = ifdLevel: attrs:
    if ifdLevel < 3
    then filterNonIfdInputsValues attrs
    else attrs;

  # Run unit tests with: nix-instantiate --eval --strict -A unit.tests
  # An empty list means success.
  unitTests =
    let
      tests = haskell-nix.callPackage ./unit.nix {};
      testsFailedEcho = lib.concatMapStringsSep "\n" (t: "echo ${t.name} failed") tests;
      testsFinalLine = if builtins.length tests == 0 then "\ntouch $out" else "\nexit 1";
      testsScript = testsFailedEcho + testsFinalLine;
    in
    runCommand "unit-tests" { passthru = { inherit tests; }; } testsScript;

  # All tests.
  allTests = {
    cabal-simple = haskell-nix.callPackage ./cabal-simple { inherit util; };
    cabal-simple-prof = haskell-nix.callPackage ./cabal-simple-prof { inherit util; };
    cabal-sublib = haskell-nix.callPackage ./cabal-sublib { inherit util; };
    cabal-22 = haskell-nix.callPackage ./cabal-22 { inherit util; };
    with-packages = haskell-nix.callPackage ./with-packages { inherit util; };
    builder-haddock = haskell-nix.callPackage ./builder-haddock {};
    stack-simple = haskell-nix.callPackage ./stack-simple {};
    stack-local-resolver = haskell-nix.callPackage ./stack-local-resolver {};
    snapshots = haskell-nix.callPackage ./snapshots {};
    shell-for = haskell-nix.callPackage ./shell-for {};
    shell-for-setup-deps = haskell-nix.callPackage ./shell-for-setup-deps {};
    setup-deps = import ./setup-deps { inherit pkgs; };
    callStackToNix = haskell-nix.callPackage ./call-stack-to-nix {};
    callCabalProjectToNix = haskell-nix.callPackage ./call-cabal-project-to-nix {};
    cabal-source-repo = haskell-nix.callPackage ./cabal-source-repo {};
    buildable = haskell-nix.callPackage ./buildable {};
    project-flags-cabal = haskell-nix.callPackage ./project-flags/cabal.nix {};
    project-flags-stack = haskell-nix.callPackage ./project-flags/stack.nix {};
    fully-static = haskell-nix.callPackage ./fully-static { inherit (pkgs) buildPackages; };
    ghc-options-cabal = haskell-nix.callPackage ./ghc-options/cabal.nix {};
    ghc-options-stack = haskell-nix.callPackage ./ghc-options/stack.nix {};
    exe-only = haskell-nix.callPackage ./exe-only { inherit util; };
    stack-source-repo = haskell-nix.callPackage ./stack-source-repo {};
    cabal-doctests = haskell-nix.callPackage ./cabal-doctests { inherit util; };

    unit = unitTests;
  };

  # This is the same as allTests, but filter out all the key/vaules from the
  # tests other than the "ifdInputs" key if the input ifdLevel is less than 3.
  allTestsRemoveIfdLTLevel3 = ifdLevel:
    filterNonIfdInputsValuesLTLevel3 ifdLevel allTests;

  # This is the same as allTestsRemoveIfdLTLevel3, but make sure
  # recurseForDerivations is set on all child values under the
  # ifdInputs key.
  allTestsWithIfdInputs = ifdLevel:
    addIfdInputsToVal (allTestsRemoveIfdLTLevel3 ifdLevel);

  # This is the same as allTestsWithIfdInputs, but returns an empty attribute set
  # if the input ifdLevel is 0 or 1.
  #
  # Here is the result based on the input ifdLevel:
  #
  # - input ifdLevel is 0 or 1: {}
  # - input ifdLevel is 2: filter out everything from the children of allTests
  #   except for the ifdInputs attribute
  # - input ifdLevel is 3 or greater: return allTests
  optionalIfdTests = ifdLevel:
    pkgs.lib.optionalAttrs (ifdLevel > 1) (allTestsWithIfdInputs ifdLevel);
in

pkgs.recurseIntoAttrs {
  haskellNixRoots = haskell-nix.haskellNixRoots' ifdLevel;
} // optionalIfdTests ifdLevel

## more possible test cases
# 1. fully static linking
# 2. cabal 2.4 stuff
# 3. cross-compiling
