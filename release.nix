# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluating
# on a machine with e.g. no way to build the Darwin IFDs you need!
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 3
, checkMaterialization ? false }:

let
  inherit (import ./ci-lib.nix) stripAttrsForHydra filterDerivations;
  genericPkgs = import (import ./nix/sources.nix).nixpkgs {};
  lib = genericPkgs.lib;
  ci = import ./ci.nix { inherit supportedSystems ifdLevel checkMaterialization; restrictEval = true; };
  allJobs = stripAttrsForHydra (filterDerivations ci);
  names = x: lib.filter (n: n != "recurseForDerivations" && n != "meta")
    (builtins.attrNames x);
in
  builtins.listToAttrs (
    lib.concatMap (nixpkgsVer:
      let nixpkgsJobs = allJobs.${nixpkgsVer};
      in lib.concatMap (compiler-nix-name:
        let ghcJobs = nixpkgsJobs.${compiler-nix-name};
        in builtins.map (platform: {
          name = "required-${nixpkgsVer}-${compiler-nix-name}-${platform}";
          value = genericPkgs.releaseTools.aggregate {
            name = "haskell.nix-${nixpkgsVer}-${compiler-nix-name}-${platform}";
            meta.description = "All ${nixpkgsVer} ${compiler-nix-name} ${platform} jobs";
            constituents = lib.collect (d: lib.isDerivation d) ghcJobs.${platform};
          };
        }) (names ghcJobs)
      ) (names nixpkgsJobs)
    ) (names allJobs))


