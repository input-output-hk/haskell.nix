# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluating
# on a machine with e.g. no way to build the Darwin IFDs you need!
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 3
, include ? (compiler-nix-name: true)
, checkMaterialization ? false }:

let
  traceNames = prefix: builtins.mapAttrs (n: v:
    if builtins.isAttrs v
      then if v ? type && v.type == "derivation"
        then __trace (prefix + n) v
        else traceNames (prefix + n + ".") v
      else v);
  inherit (import ./ci-lib.nix { pkgs = genericPkgs; }) stripAttrsForHydra filterDerivations;
  genericPkgs = (import ./. {}).pkgs;
  lib = genericPkgs.lib;
  ci = import ./ci.nix { inherit supportedSystems ifdLevel checkMaterialization; restrictEval = true; };
  allJobs = stripAttrsForHydra (filterDerivations ci);
  latestJobs = lib.optionalAttrs (include "ghc8107") {
    # All the jobs are included in the `requiredJobs`, but the ones
    # added here will also included without aggregation, making it easier
    # to find a failing test.  Keep in mind though that adding too many
    # of these will slow down eval times.
    x86_64-linux = allJobs.unstable.ghc8107.x86_64-linux.native or {};
    x86_64-darwin = allJobs.unstable.ghc8107.x86_64-darwin.native or {};
  };
  names = x: lib.filter (n: n != "recurseForDerivations" && n != "meta")
    (builtins.attrNames x);
  requiredJobs =
    builtins.listToAttrs (
      lib.concatMap (nixpkgsVer:
        let nixpkgsJobs = allJobs.${nixpkgsVer};
        in lib.concatMap (compiler-nix-name:
          let ghcJobs = nixpkgsJobs.${compiler-nix-name};
          in lib.optionals (include compiler-nix-name) (builtins.concatMap (platform:
            let platformJobs = ghcJobs.${platform};
            in builtins.map (crossPlatform: {
              name = "required-${nixpkgsVer}-${compiler-nix-name}-${platform}-${crossPlatform}";
              value = genericPkgs.releaseTools.aggregate {
                name = "haskell.nix-${nixpkgsVer}-${compiler-nix-name}-${platform}-${crossPlatform}";
                meta.description = "All ${nixpkgsVer} ${compiler-nix-name} ${platform} ${crossPlatform} jobs";
                constituents = lib.collect (d: lib.isDerivation d) platformJobs.${crossPlatform};
              };
           }) (names platformJobs)
         ) (names ghcJobs))
        ) (names nixpkgsJobs)
      ) (names allJobs));
in traceNames "job " (latestJobs // requiredJobs // {
    windows-secp256k1 =
      let
        pkgs = (import ./. {}).pkgs-unstable; 
        makeBinDist = drv: pkgs.runCommand drv.name {
          nativeBuildInputs = [ pkgs.zip ];
        } ''
          mkdir -p $out/nix-support
          cp -r ${drv}/* .
          chmod -R +w .
          zip -r $out/${drv.name}.zip .
          echo "file binary-dist $out/${drv.name}.zip" > $out/nix-support/hydra-build-products
       '';
      in makeBinDist pkgs.pkgsCross.mingwW64.secp256k1;
    required = genericPkgs.releaseTools.aggregate {
      name = "haskell.nix-required";
      meta.description = "All jobs required to pass CI";
      # Using the names here requires https://github.com/NixOS/hydra/issues/715
      constituents = builtins.attrNames requiredJobs;
    };
 })

