{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, scrubJobs ? true
, haskell-nix ? { outPath = ./.; rev = "abcdef"; }
, nixpkgsArgs ? {}
}:

let fixedNixpkgs = import ./nixpkgs {}; in
with fixedNixpkgs.lib;
let
  # Remove tests which have meta.disabled = true
  filterTests = let
    nonEmpty = attrs: length (attrValues attrs) != 0;
    removeDisabled = filterAttrs (system: test: !(test.meta.disabled or false));
  in jobs: jobs // {
    tests = filterAttrs (_: nonEmpty) (mapAttrs (name: removeDisabled) jobs.tests);
  };

  inherit (systems.examples) musl64 mingwW64;

  jobs = nixpkgs-pin:
    with (import (fixedNixpkgs.path + "/pkgs/top-level/release-lib.nix") {
      inherit supportedSystems scrubJobs nixpkgsArgs;
      packageSet = {
            system ? builtins.currentSystem
          , crossSystem ? null
          , nixpkgsArgs ? { inherit system crossSystem; }
          , ...}@args:
        import (haskell-nix + /build.nix) (args // {
          nixpkgsArgs = nixpkgsArgs // { inherit nixpkgs-pin; };
      });
    });

    {
      native = filterTests (mapTestOn (packagePlatforms pkgs));
      # Disabled for now. Something is wrong and this would require `allowBroken`
      # "${musl64.config}" = filterTests (mapTestOnCross musl64 (packagePlatforms pkgs));
    } // (optionalAttrs (nixpkgs-pin == "release-19.03") {
      "${mingwW64.config}" = filterTests (mapTestOnCross mingwW64 (packagePlatforms pkgs));
    });

  allJobs =
    builtins.mapAttrs (_: nixpkgs-pin: jobs nixpkgs-pin) {
      "R1903" = "release-19.03";
      "R1909" = "release-19.09";
    };

in allJobs // {
    # On IOHK Hydra, "required" is a special job that updates the
    # GitHub CI status.
    required = fixedNixpkgs.releaseTools.aggregate {
      name = "haskell.nix-required";
      meta.description = "All jobs required to pass CI";
      constituents =
          collect isDerivation allJobs.R1903.native
       ++ collect isDerivation allJobs.R1909.native;
    };
  }


