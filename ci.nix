# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluating
# on a machine with e.g. no way to build the Darwin IFDs you need!
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 3
# Whether or not we are evaluating in restricted mode. This is true in Hydra, but not in Hercules.
, restrictEval ? false
, checkMaterialization ? false }:
 let
  inherit (import ./ci-lib.nix) dimension platformFilterGeneric filterAttrsOnlyRecursive;
  sources = import ./nix/sources.nix {};
  nixpkgsVersions = {
    "R2009" = "nixpkgs-2009";
    "R2105" = "nixpkgs-2105";
    "unstable" = "nixpkgs-unstable";
  };
  compilerNixNames = nixpkgsName: nixpkgs: builtins.mapAttrs (compiler-nix-name: runTests: {
    inherit (import ./default.nix { inherit checkMaterialization; }) nixpkgsArgs;
    inherit runTests;
  }) (
    # GHC version to cache and whether to run the tests against them.
    # This list of GHC versions should include everything for which we
    # have a ./materialized/ghcXXX directory containing the materialized
    # cabal-install and nix-tools plans.  When removing a ghc version
    # from here (so that is no longer cached) also remove ./materialized/ghcXXX.
    # Update supported-ghc-versions.md to reflect any changes made here.
    nixpkgs.lib.optionalAttrs (nixpkgsName == "R2009") {
      ghc865 = false;
      ghc8105 = false;
    } // nixpkgs.lib.optionalAttrs (nixpkgsName == "R2105") {
      ghc865 = false;
      ghc8105 = true;
    } // nixpkgs.lib.optionalAttrs (nixpkgsName == "unstable") {
      ghc865 = false;
      ghc884 = false; # Native version is used to boot 9.0.1
      ghc8105 = true;
      ghc901 = true;
      ghc810420210212 = false;
    });
  systems = nixpkgs: nixpkgs.lib.filterAttrs (_: v: builtins.elem v supportedSystems) {
    # I wanted to take these from 'lib.systems.examples', but apparently there isn't one for linux!
    linux = "x86_64-linux";
    darwin = "x86_64-darwin";
  };
  crossSystems = nixpkgsName: nixpkgs: compiler-nix-name: system:
    # We need to use the actual nixpkgs version we're working with here, since the values
    # of 'lib.systems.examples' are not understood between all versions
    let lib = nixpkgs.lib;
    in lib.optionalAttrs (nixpkgsName == "unstable" && (__elem compiler-nix-name ["ghc8105"])) {
    inherit (lib.systems.examples) ghcjs;
  } // lib.optionalAttrs (system == "x86_64-linux" &&
         nixpkgsName == "unstable" && (__elem compiler-nix-name ["ghc8105"])) {
    # Windows cross compilation is currently broken on macOS
    inherit (lib.systems.examples) mingwW64;
  } // lib.optionalAttrs (system == "x86_64-linux" && nixpkgsName == "unstable" && compiler-nix-name == "ghc8105") {
    # Musl cross only works on linux
    # aarch64 cross only works on linux
    inherit (lib.systems.examples) musl64 aarch64-multiplatform;
  };
  isDisabled = d: d.meta.disabled or false;
in
dimension "Nixpkgs version" nixpkgsVersions (nixpkgsName: nixpkgs-pin:
  let pinnedNixpkgsSrc = sources.${nixpkgs-pin};
      # We need this for generic nixpkgs stuff at the right version
      genericPkgs = import pinnedNixpkgsSrc {};
  in dimension "GHC version" (compilerNixNames nixpkgsName genericPkgs) (compiler-nix-name: {nixpkgsArgs, runTests}:
    dimension "System" (systems genericPkgs) (systemName: system:
      let pkgs = import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system; });
          build = import ./build.nix { inherit pkgs ifdLevel compiler-nix-name; };
          platformFilter = platformFilterGeneric pkgs system;
      in filterAttrsOnlyRecursive (_: v: platformFilter v && !(isDisabled v)) ({
        # Native builds
        # TODO: can we merge this into the general case by picking an appropriate "cross system" to mean native?
        native = pkgs.recurseIntoAttrs ({
          roots = pkgs.haskell-nix.roots' compiler-nix-name ifdLevel;
          ghc = pkgs.buildPackages.haskell-nix.compiler."${compiler-nix-name}";
        } // pkgs.lib.optionalAttrs runTests {
          inherit (build) tests tools maintainer-scripts maintainer-script-cache;
        } // pkgs.lib.optionalAttrs (ifdLevel >= 1) {
          iserv-proxy = pkgs.ghc-extra-projects."${compiler-nix-name}".getComponent "iserv-proxy:exe:iserv-proxy";
        } // pkgs.lib.optionalAttrs (ifdLevel >= 3) {
          hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; inherit compiler-nix-name; }).getComponent "exe:hello";
        });
      }
      //
      dimension "Cross system" (crossSystems nixpkgsName genericPkgs compiler-nix-name system) (crossSystemName: crossSystem:
        # Cross builds
        let pkgs = import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system crossSystem; });
            build = import ./build.nix { inherit pkgs ifdLevel compiler-nix-name; };
        in pkgs.recurseIntoAttrs (pkgs.lib.optionalAttrs (ifdLevel >= 1) ({
            roots = pkgs.haskell-nix.roots' compiler-nix-name ifdLevel;
            ghc = pkgs.buildPackages.haskell-nix.compiler."${compiler-nix-name}";
            # TODO: look into cross compiling ghc itself
            # ghc = pkgs.haskell-nix.compiler."${compiler-nix-name}";
            # TODO: look into making tools work when cross compiling
            # inherit (build) tools;
          } // pkgs.lib.optionalAttrs (runTests && crossSystemName != "aarch64-multiplatform") {
            # Tests are broken on aarch64 cross https://github.com/input-output-hk/haskell.nix/issues/513
            inherit (build) tests;
        }) // pkgs.lib.optionalAttrs (ifdLevel >= 2 && crossSystemName != "ghcjs") {
          # GHCJS builds its own template haskell runner.
          remote-iserv = pkgs.ghc-extra-projects."${compiler-nix-name}".getComponent "remote-iserv:exe:remote-iserv";
          iserv-proxy = pkgs.ghc-extra-projects."${compiler-nix-name}".getComponent "iserv-proxy:exe:iserv-proxy";
        } // pkgs.lib.optionalAttrs (ifdLevel >= 3) {
          hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; inherit compiler-nix-name; }).getComponent "exe:hello";
        })
      ))
    )
  )
)
