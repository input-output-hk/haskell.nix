{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, scrubJobs ? true
, haskell-nix ? { outPath = ./.; rev = "abcdef"; }
, nixpkgsArgs ? {}
, ifdLevel ? 3
}:

let defaultNixpkgs = import ./nixpkgs {}; in
with defaultNixpkgs.lib;
let
  # Remove tests which have meta.disabled = true
  filterTests = let
    nonEmpty = attrs: length (attrValues attrs) != 0;
    removeDisabled = filterAttrs (system: test: !(test.meta.disabled or false));
  in jobs: jobs // {
    tests = filterAttrs (_: nonEmpty) (mapAttrs (name: removeDisabled) jobs.tests);
  };

  mkPins = inputs: defaultNixpkgs.runCommand "ifd-pins" {} ''
    mkdir $out
    cd $out
    ${concatMapStringsSep "\n" (input: "ln -sv ${input.value} ${input.key}") (attrValues (mapAttrs (key: value: { inherit key value; }) inputs))}
  '';

  jobs = nixpkgs-pin:
    # Now we know what nixpkgs-pin we are testing use that one.
    # This fixes and issue where the vendor has changed from `x86_64-pc-mingw32`
    # to `x86_64-w64-mingw32` in 19.09.
    let pinnedNixpkgs = import ./nixpkgs { inherit nixpkgs-pin; }; in
    with pinnedNixpkgs.lib;
    let inherit (systems.examples) musl64 mingwW64; in
    with (import (pinnedNixpkgs.path + "/pkgs/top-level/release-lib.nix") {
      inherit supportedSystems scrubJobs nixpkgsArgs;
      packageSet = {
            system ? builtins.currentSystem
          , crossSystem ? null
          , nixpkgsArgs ? { inherit system crossSystem; }
          , ...}@args:
        import (haskell-nix + /build.nix) (args // {
          nixpkgsArgs = nixpkgsArgs // { inherit nixpkgs-pin; };
          inherit ifdLevel;
      });
    });

    {
      native = filterTests (mapTestOn (packagePlatforms pkgs));
      # Disabled for now. Something is wrong and this would require `allowBroken`
      # "${musl64.config}" = filterTests (mapTestOnCross musl64 (packagePlatforms pkgs));
      "${mingwW64.config}" = filterTests (mapTestOnCross mingwW64 (packagePlatforms pkgs));
    };

  allJobs =
    builtins.mapAttrs (_: nixpkgs-pin: jobs nixpkgs-pin) {
      "R1903" = "release-19.03";
      "R1909" = "release-19.09";
    };

in allJobs // {
    ifd-pins = mkPins {
      # hydra is not aware of anything depended on at eval time (via IFD), so it wont try to cache those things
      # this job will expose all of those derivations as deps of the `ifd-pins` derivation, which tricks hydra into caching them
      # that will then speed up evals for end-users that are using hydra as a cache
      old-ghc-nix = let spec = __fromJSON (__readFile ./compiler/old-ghc-nix/old-ghc-nix.json); in defaultNixpkgs.fetchgit {
        name = "old-ghc-nix";
        inherit (spec) sha256 url rev;
      };
    };
    # On IOHK Hydra, "required" is a special job that updates the
    # GitHub CI status.
    required = defaultNixpkgs.releaseTools.aggregate {
      name = "haskell.nix-required";
      meta.description = "All jobs required to pass CI";
      constituents =
          collect isDerivation allJobs.R1903.native
       ++ collect isDerivation allJobs.R1909.native;
    };
  }


