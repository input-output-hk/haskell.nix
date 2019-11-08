{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, scrubJobs ? true
, haskell-nix ? { outPath = ./.; rev = "abcdef"; }
, nixpkgsArgs ? {}
}:

# Just checking hydra (TODO delete this and put arg back)
let fixedNixpkgs = import (builtins.fetchTarball {
    url = "https://github.com/input-output-hk/haskell.nix/archive/1c85c0561ab93bcd16616ff0b32ee59689d0344d.tar.gz";
    sha256 = "0vqd12kv4gwzc49i7rmii6ixlfrdwnbjvmibcj4yr64k1sspcgbn";
  } + "/nixpkgs") {}; in
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

  inherit (systems.examples) musl64;

  jobs = {
    native = filterTests (mapTestOn (packagePlatforms pkgs));
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
