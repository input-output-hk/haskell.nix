# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluting
# on a machine with e.g. no way to build the Darwin IFDs you need! 
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 3 }:

let
  genericPkgs = import ./nixpkgs {};
  lib = genericPkgs.lib;
  # Hydra doesn't like these attributes hanging around in "jobsets": it thinks they're jobs!
  stripAttrsForHydra = lib.filterAttrsRecursive (n: _: n != "recurseForDerivations" && n != "dimension");
  allJobs = stripAttrsForHydra (import ./ci.nix { inherit supportedSystems ifdLevel; restrictEval = true; });
in allJobs // {
    # On IOHK Hydra, "required" is a special job that updates the
    # GitHub CI status.
    required = genericPkgs.releaseTools.aggregate {
      name = "haskell.nix-required";
      meta.description = "All jobs required to pass CI";
      # Just include everything: Hercules will require them all too!
      constituents = lib.collect lib.isDerivation allJobs;
    };
  }


