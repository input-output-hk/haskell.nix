{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, scrubJobs ? true
, haskell-nix ? { outPath = ./.; rev = "abcdef"; }
, nixpkgsArgs ? {}
}:

let fixedNixpkgs = import ./nixpkgs {}; in
with fixedNixpkgs.lib;
with (import (fixedNixpkgs.path + "/pkgs/top-level/release-lib.nix") {
  inherit supportedSystems scrubJobs nixpkgsArgs;
  packageSet = import (haskell-nix + /build.nix);
});

let
  # Remove tests which have meta.disabled = true
  filterTests = let
    nonEmpty = attrs: length (attrValues attrs) != 0;
    removeDisabled = filterAttrs (system: test: !(test.meta.disabled or false));
  in jobs: jobs // {
    tests = filterAttrs (_: nonEmpty) (mapAttrs (name: removeDisabled) jobs.tests);
  };

  inherit (systems.examples) musl64 mingwW64;

  jobs = {
    native = filterTests (mapTestOn (packagePlatforms pkgs));
    "${mingwW64.config}" = filterTests (mapTestOnCross mingwW64 (packagePlatforms pkgs));
    # Disabled for now. Something is wrong and this would require `allowBroken`
    # "${musl64.config}" = filterTests (mapTestOnCross musl64 (packagePlatforms pkgs));
  } // {
    # On IOHK Hydra, "required" is a special job that updates the
    # GitHub CI status.
    required = fixedNixpkgs.releaseTools.aggregate {
      name = "haskell.nix-required";
      meta.description = "All jobs required to pass CI";
      constituents = collect isDerivation jobs.native;
    };
  };

in jobs
