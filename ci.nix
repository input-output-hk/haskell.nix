# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluating
# on a machine with e.g. no way to build the Darwin IFDs you need!
{ ifdLevel ? 3
# Whether or not we are evaluating in restricted mode. This is true in Hydra, but not in Hercules.
, restrictEval ? false
, checkMaterialization ? false
, compat
, system
, evalSystem ? builtins.currentSystem or "x86_64-linux"
, pkgs ? (compat { inherit system; }).pkgs }:
 let
  inherit (import ./ci-lib.nix { inherit pkgs; }) dimension platformFilterGeneric filterAttrsOnlyRecursive;
  inherit (pkgs.haskell-nix) sources;
  nixpkgsVersions = {
    "R2205" = "nixpkgs-2205";
    "R2211" = "nixpkgs-2211";
    "R2305" = "nixpkgs-2305";
    "unstable" = "nixpkgs-unstable";
  };
  haskellNix = compat { inherit checkMaterialization system; };
  nixpkgsArgs = haskellNix.nixpkgsArgs // {
    # Needed for dwarf tests
    config = haskellNix.nixpkgsArgs.config // {
      permittedInsecurePackages = ["libdwarf-20210528" "libdwarf-20181024" "dwarfdump-20181024"];
    };
  };
  compilerNixNames = nixpkgsName: nixpkgs:
    # Include only the GHC versions that are supported by haskell.nix
    nixpkgs.lib.filterAttrs (compiler-nix-name: _:
        # We have less x86_64-darwin build capacity so build fewer GhC versions
        (system != "x86_64-darwin" || (
           !__elem compiler-nix-name ["ghc8104" "ghc810420210212" "ghc8105" "ghc8106" "ghc901" "ghc921" "ghc922"]))
      &&
        # aarch64-darwin requires ghc 8.10.7
        (system != "aarch64-darwin" || (
           !__elem compiler-nix-name ["ghc865" "ghc884" "ghc8104" "ghc810420210212" "ghc8105" "ghc8106" "ghc901" "ghc921" "ghc922"]))
      &&
        # aarch64-linux requires ghc 8.8.4
        (system != "aarch64-linux" || (
           !__elem compiler-nix-name ["ghc865" "ghc8104" "ghc810420210212" "ghc8105" "ghc8106" "ghc901" "ghc921" "ghc922"]
        )))
    (builtins.mapAttrs (compiler-nix-name: runTests: {
      inherit runTests;
    }) (
      # GHC version to cache and whether to run the tests against them.
      # This list of GHC versions should include everything for which we
      # have a ./materialized/ghcXXX directory containing the materialized
      # cabal-install and nix-tools plans.  When removing a ghc version
      # from here (so that is no longer cached) also remove ./materialized/ghcXXX.
      # Update supported-ghc-versions.md to reflect any changes made here.
      nixpkgs.lib.optionalAttrs (nixpkgsName == "R2211") {
        ghc8107 = false;
        ghc902 = false;
        ghc928 = false;
        ghc945 = false;
      } // nixpkgs.lib.optionalAttrs (nixpkgsName == "R2305") {
        ghc8107 = false;
        ghc902 = false;
        ghc928 = false;
        ghc945 = false;
        ghc962 = false;
      } // nixpkgs.lib.optionalAttrs (nixpkgsName == "unstable") {
        ghc884 = false;
        ghc8107 = true;
        ghc902 = false;
        ghc928 = true;
        ghc945 = true;
        ghc962 = true;
      }));
  crossSystems = nixpkgsName: nixpkgs: compiler-nix-name:
    # We need to use the actual nixpkgs version we're working with here, since the values
    # of 'lib.systems.examples' are not understood between all versions
    let lib = nixpkgs.lib;
    in lib.optionalAttrs (nixpkgsName == "unstable"
      && ((system == "x86_64-linux"  && __elem compiler-nix-name ["ghc8107" "ghc962"])
       || (system == "aarch64-linux" && __elem compiler-nix-name ["ghc8107" "ghc962"])
       || (system == "x86_64-darwin" && __elem compiler-nix-name ["ghc8107" "ghc962"])
       || (system == "aarch64-darwin" && __elem compiler-nix-name ["ghc8107" "ghc962"])
       )) {
    inherit (lib.systems.examples) ghcjs;
  } // lib.optionalAttrs (nixpkgsName == "unstable"
      && ((system == "x86_64-linux"  && __elem compiler-nix-name ["ghc8107" "ghc902" "ghc926" "ghc927" "ghc928" "ghc945" "ghc962"])
       || (system == "x86_64-darwin" && __elem compiler-nix-name []))) { # TODO add ghc versions when we have more darwin build capacity
    inherit (lib.systems.examples) mingwW64;
  } // lib.optionalAttrs (system == "x86_64-linux" && nixpkgsName == "unstable" && __elem compiler-nix-name ["ghc8107" "ghc902" "ghc922" "ghc923" "ghc924" "ghc926" "ghc927" "ghc928" "ghc945" "ghc962"]) {
    # Musl cross only works on linux
    # aarch64 cross only works on linux
    inherit (lib.systems.examples) musl64 aarch64-multiplatform;
  } // lib.optionalAttrs (system == "x86_64-linux" && nixpkgsName == "unstable" && __elem compiler-nix-name ["ghc927" "ghc928"]) {
    # TODO fix this for the compilers we build with hadrian (ghc >=9.4)
    inherit (lib.systems.examples) aarch64-multiplatform-musl;
  } // lib.optionalAttrs (system == "aarch64-linux" && nixpkgsName == "unstable" && __elem compiler-nix-name ["ghc927" "ghc928" "ghc945" "ghc962"]) {
    inherit (lib.systems.examples) aarch64-multiplatform-musl;
  };
  isDisabled = d: d.meta.disabled or false;
in
dimension "Nixpkgs version" nixpkgsVersions (nixpkgsName: nixpkgs-pin:
  let pinnedNixpkgsSrc = sources.${nixpkgs-pin};
      evalPackages = import pinnedNixpkgsSrc (nixpkgsArgs // { system = evalSystem; });
  in dimension "GHC version" (compilerNixNames nixpkgsName evalPackages) (compiler-nix-name: {runTests}:
      let pkgs = import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system; });
          build = import ./build.nix { inherit pkgs evalPackages ifdLevel compiler-nix-name haskellNix; };
          platformFilter = platformFilterGeneric pkgs system;
      in filterAttrsOnlyRecursive (_: v: platformFilter v && !(isDisabled v)) ({
        # Native builds
        # TODO: can we merge this into the general case by picking an appropriate "cross system" to mean native?
        native = pkgs.recurseIntoAttrs ({
          roots = pkgs.haskell-nix.roots' compiler-nix-name ifdLevel;
          ghc = pkgs.buildPackages.haskell-nix.compiler.${compiler-nix-name};
        } // pkgs.lib.optionalAttrs runTests {
          inherit (build) tests tools maintainer-scripts maintainer-script-cache;
        } // pkgs.lib.optionalAttrs (ifdLevel >= 1) {
          inherit (pkgs.haskell-nix.iserv-proxy-exes.${compiler-nix-name}) iserv-proxy;
        } // pkgs.lib.optionalAttrs (ifdLevel >= 3) {
          hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; inherit evalPackages compiler-nix-name; }).getComponent "exe:hello";
        });
      }
      //
      dimension "Cross system" (crossSystems nixpkgsName evalPackages compiler-nix-name) (crossSystemName: crossSystem:
        # Cross builds
        let pkgs = import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system crossSystem; });
            build = import ./build.nix { inherit pkgs evalPackages ifdLevel compiler-nix-name haskellNix; };
        in pkgs.recurseIntoAttrs (pkgs.lib.optionalAttrs (ifdLevel >= 1) ({
            roots = pkgs.haskell-nix.roots' compiler-nix-name ifdLevel;
            ghc = pkgs.buildPackages.haskell-nix.compiler.${compiler-nix-name};
            # TODO: look into cross compiling ghc itself
            # ghc = pkgs.haskell-nix.compiler.${compiler-nix-name};
            # TODO: look into making tools work when cross compiling
            # inherit (build) tools;
          } // pkgs.lib.optionalAttrs (runTests && crossSystemName != "aarch64-multiplatform") {
            # Tests are broken on aarch64 cross https://github.com/input-output-hk/haskell.nix/issues/513
            inherit (build) tests;
        })
        # GHCJS builds its own template haskell runner.
        // pkgs.lib.optionalAttrs (ifdLevel >= 2 && crossSystemName != "ghcjs")
            pkgs.haskell-nix.iserv-proxy-exes.${compiler-nix-name}
        // pkgs.lib.optionalAttrs (ifdLevel >= 3) {
          hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; inherit compiler-nix-name; }).getComponent "exe:hello";
        })
      ))
    )
  )
