# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluating
# on a machine with e.g. no way to build the Darwin IFDs you need!
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 0
, checkMaterialization ? false }:

let
  inherit (import ./ci-lib.nix) stripAttrsForHydra filterDerivations;
  genericPkgs = import (import ./nix/sources.nix).nixpkgs {};
  lib = genericPkgs.lib;
  ci = import ./ci.nix { inherit supportedSystems ifdLevel checkMaterialization; restrictEval = true; };
  allJobs = stripAttrsForHydra (filterDerivations ci);
  latestJobs = {
    # All the jobs are included in the `requiredJobs`, but the ones
    # added here will also included without aggregation, making it easier
    # to find a failing test.  Keep in mind though that adding too many
    # of these will slow down eval times.
    linux = allJobs.R2009.ghc8105.linux.native or {};
    darwin = allJobs.R2009.ghc8105.darwin.native or {};
  };
  names = x: lib.filter (n: n != "recurseForDerivations" && n != "meta")
    (builtins.attrNames x);
  requiredJobs =
    builtins.listToAttrs (
      lib.concatMap (nixpkgsVer:
        let nixpkgsJobs = allJobs.${nixpkgsVer};
        in lib.concatMap (compiler-nix-name:
          let ghcJobs = nixpkgsJobs.${compiler-nix-name};
          in builtins.concatMap (platform:
            let platformJobs = ghcJobs.${platform};
            in builtins.map (crossPlatform: {
              name = "required-${nixpkgsVer}-${compiler-nix-name}-${platform}-${crossPlatform}";
              value = genericPkgs.releaseTools.aggregate {
                name = "haskell.nix-${nixpkgsVer}-${compiler-nix-name}-${platform}-${crossPlatform}";
                meta.description = "All ${nixpkgsVer} ${compiler-nix-name} ${platform} ${crossPlatform} jobs";
                constituents = lib.collect (d: lib.isDerivation d) platformJobs.${crossPlatform};
              };
           }) (names platformJobs)
         ) (names ghcJobs)
        ) (names nixpkgsJobs)
      ) (names allJobs));
in latestJobs // requiredJobs // {
    required = genericPkgs.releaseTools.aggregate {
      name = "haskell.nix-required";
      meta.description = "All jobs required to pass CI";
      # Using the names here requires https://github.com/NixOS/hydra/issues/715
      constituents = builtins.attrNames requiredJobs;
    };
 }


