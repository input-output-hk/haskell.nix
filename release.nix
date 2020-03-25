# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluting
# on a machine with e.g. no way to build the Darwin IFDs you need! 
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 3 }:

let
  inherit (import ./ci-lib.nix) stripAttrsForHydra filterDerivations;
  genericPkgs = import ./nixpkgs {};
  lib = genericPkgs.lib;
  ci = import ./ci.nix { inherit supportedSystems ifdLevel; restrictEval = true; };
  allJobs = stripAttrsForHydra (filterDerivations ci);
in allJobs // {
    # On IOHK Hydra, "required" is a special job that updates the
    # GitHub CI status.
    required = genericPkgs.releaseTools.aggregate {
      name = "haskell.nix-required";
      meta.description = "All jobs required to pass CI";
      # Hercules will require all of these, we just require the 1909 jobs
      # to avoid stressing Hydra too much
      constituents = lib.collect lib.isDerivation allJobs.R1909.linux.native;
    };
  }


