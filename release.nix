# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluating
# on a machine with e.g. no way to build the Darwin IFDs you need!
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 2
, checkMaterialization ? false }:

let
  inherit (import ./ci-lib.nix) stripAttrsForHydra filterDerivations;
  genericPkgs = import (import ./nix/sources.nix).nixpkgs {};
  lib = genericPkgs.lib;
  ci = import ./ci.nix { inherit supportedSystems ifdLevel checkMaterialization; restrictEval = true; };
  allJobs = stripAttrsForHydra (filterDerivations ci);
in rec {
    r2009 = genericPkgs.releaseTools.aggregate {
      name = "haskell.nix-r2009";
      meta.description = "All 20.09 jobs";
      constituents = lib.collect (d: lib.isDerivation d) allJobs.R2009.ghc865;
    };
#    r2003 = genericPkgs.releaseTools.aggregate {
#      name = "haskell.nix-r2003";
#      meta.description = "All 20.03 jobs";
#      constituents = lib.collect (d: lib.isDerivation d) allJobs.R2003.ghc865;
#    };
#    # On IOHK Hydra, "required" is a special job that updates the
#    # GitHub CI status.
#    required = genericPkgs.releaseTools.aggregate {
#      name = "haskell.nix-required";
#      meta.description = "All jobs required to pass CI";
#      constituents = [ r2003 r2009 ];
#    };
  }


