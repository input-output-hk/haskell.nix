# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluting
# on a machine with e.g. no way to build the Darwin IFDs you need! 
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 3 }:

let
  genericPkgs = import ./nixpkgs {};
  lib = genericPkgs.lib;
  ci = import ./ci.nix { inherit supportedSystems ifdLevel; restrictEval = true; };
  # Hydra doesn't like these attributes hanging around in "jobsets": it thinks they're jobs!
  stripAttrsForHydra = filterAttrsOnlyRecursive (n: _: n != "recurseForDerivations" && n != "dimension");
  # Keep derivations and attrsets with 'recurseForDerivations'. This ensures that we match the
  # derivations that Hercules will see, and prevents Hydra from trying to pick up all sorts of bad stuff
  # (like attrsets that contain themselves!).
  filterDerivations = filterAttrsOnlyRecursive (n: attrs: lib.isDerivation attrs || attrs.recurseForDerivations or false);
  # A version of 'filterAttrsRecursive' that doesn't recurse into derivations. This prevents us from going into an infinite
  # loop with the 'out' attribute on derivations.
  # TODO: Surely this shouldn't be necessary. I think normal 'filterAttrsRecursive' will effectively cause infinite loops
  # if you keep derivations and your predicate forces the value of the attribute, as this then triggers a loop on the
  # 'out' attribute. Weird.
  filterAttrsOnlyRecursive = pred: set:
    lib.listToAttrs (
      lib.concatMap (name:
        let v = set.${name}; in
        if pred name v then [
          (lib.nameValuePair name (
            if builtins.isAttrs v && !lib.isDerivation v then filterAttrsOnlyRecursive pred v
            else v
          ))
        ] else []
      ) (builtins.attrNames set)
    );
  allJobs = stripAttrsForHydra (filterDerivations ci);
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


