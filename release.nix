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
in allJobs // {
    # On IOHK Hydra, "required" is a special job that updates the
    # GitHub CI status.
    required = genericPkgs.releaseTools.aggregate {
      name = "haskell.nix-required";
      meta.description = "All jobs required to pass CI";
      # Hercules will require all of these, we just require the 20.03 jobs
      # to avoid stressing Hydra too much
      constituents = lib.collect lib.isDerivation allJobs.R2003.ghc865.linux.native;
    };
  }


