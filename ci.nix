# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluating
# on a machine with e.g. no way to build the Darwin IFDs you need!
{ ifdLevel # This is passed in from flake.nix
, checkMaterialization ? false
, system ? builtins.currentSystem
, evalSystem ? builtins.currentSystem or "x86_64-linux"
  # NOTE: we apply checkMaterialization when defining nixpkgsArgs
, haskellNix ? import ./default.nix { inherit system ; }
}:
 let
  inherit (haskellNix) inputs;
  inherit (inputs.nixpkgs) lib;
  inherit
    (import ./ci-lib.nix { inherit lib; })
    dimension
    platformFilterGeneric
    filterAttrsOnlyRecursive;

  # short names for nixpkgs versions
  nixpkgsVersions = {
    "R2205" = inputs.nixpkgs-2205;
    "R2211" = inputs.nixpkgs-2211;
    "R2305" = inputs.nixpkgs-2305;
    "R2311" = inputs.nixpkgs-2311;
    "R2405" = inputs.nixpkgs-2405;
    "unstable" = inputs.nixpkgs-unstable;
  };

  nixpkgsArgs = {
    # set checkMaterialization as per top-level argument
    overlays = [
      haskellNix.overlay
      (final: prev: {
        haskell-nix = prev.haskell-nix // {
          inherit checkMaterialization;
          extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings or {} // {
            "libsodium" = [ "libsodium-18" ];
          };
        };
        libsodium-18 = (final.callPackage (inputs.nixpkgs-2311 + "/pkgs/development/libraries/libsodium") {}).overrideAttrs (_: { dontDisableStatic = true; });
      })
    ];
    # Needed for dwarf tests
    config = haskellNix.config // {
      permittedInsecurePackages = [
        "libdwarf-20210528"
        "libdwarf-20181024"
        "dwarfdump-20181024"
      ];
    };
  };

  compilerNixNames = nixpkgsName: nixpkgs: builtins.listToAttrs (
    (lib.mapAttrsToList (compiler-nix-name: runTests: {
      name = nixpkgs.haskell-nix.resolve-compiler-name compiler-nix-name;
      value = { inherit runTests; };
    }) (
      # GHC version to cache and whether to run the tests against them.
      # This list of GHC versions should include everything for which we
      # have a ./materialized/ghcXXX directory containing the materialized
      # cabal-install and nix-tools plans.  When removing a ghc version
      # from here (so that is no longer cached) also remove ./materialized/ghcXXX.
      # Update supported-ghc-versions.md to reflect any changes made here.
      nixpkgs.lib.optionalAttrs (nixpkgsName == "R2405") {
        ghc96 = false;
        ghc98 = false;
      } // nixpkgs.lib.optionalAttrs (nixpkgsName == "unstable") {
        ghc810 = true;
        ghc92 = false;
        ghc94 = false;
        ghc96 = true;
        ghc98 = true;
        ghc98llvm = false;
        ghc910 = true;
        ghc910llvm = true;
        ghc912X = true;
        ghc913 = true;
      })));
  crossSystems = nixpkgsName: nixpkgs: compiler-nix-name:
    # We need to use the actual nixpkgs version we're working with here, since the values
    # of 'lib.systems.examples' are not understood between all versions
    let lib = nixpkgs.lib;
    in lib.optionalAttrs (nixpkgsName == "unstable"
      && (__match ".*llvm" compiler-nix-name == null)
      && ((system == "x86_64-linux"  && !builtins.elem compiler-nix-name ["ghc902" "ghc928" "ghc948"])
       || (system == "aarch64-linux" && !builtins.elem compiler-nix-name ["ghc902" "ghc928" "ghc948"])
       || (system == "x86_64-darwin" && !builtins.elem compiler-nix-name ["ghc902" "ghc928" "ghc948" "ghc966" "ghc982" "ghc983" "ghc984"])
       || (system == "aarch64-darwin" && !builtins.elem compiler-nix-name ["ghc902" "ghc928" "ghc948" "ghc966" "ghc982" "ghc983" "ghc984"])
       )) {
    inherit (lib.systems.examples) ghcjs;
  } // lib.optionalAttrs (
         (__match ".*llvm" compiler-nix-name == null)
      && ((system == "x86_64-linux"  && !builtins.elem compiler-nix-name ["ghc902" "ghc928"]) # Including GHC HEAD here because the patches for rts/RtsSymbols.c no longer apply and mingwW64 GHC build fails without them
       || (system == "x86_64-darwin" && builtins.elem compiler-nix-name []))) { # TODO add ghc versions when we have more darwin build capacity
    inherit (lib.systems.examples) mingwW64;
  } // lib.optionalAttrs (nixpkgsName == "unstable"
      && (__match ".*llvm" compiler-nix-name == null)
      && ((system == "x86_64-linux"  && !builtins.elem compiler-nix-name ["ghc8107" "ghc902" "ghc928" "ghc948"])
       || (system == "x86_64-darwin" && builtins.elem compiler-nix-name []))) { # TODO add ghc versions when we have more darwin build capacity
    inherit (lib.systems.examples) ucrt64;
  } // lib.optionalAttrs (system == "x86_64-linux" && nixpkgsName == "unstable" && !builtins.elem compiler-nix-name ["ghc902" "ghc928" "ghc948"]) {
    # Musl cross only works on linux
    # aarch64 cross only works on linux
    inherit (lib.systems.examples) musl64 aarch64-multiplatform;
  } // lib.optionalAttrs (system == "x86_64-linux" && nixpkgsName == "unstable" && builtins.elem compiler-nix-name ["ghc927" "ghc928"]) {
    # TODO fix this for the compilers we build with hadrian (ghc >=9.4)
    inherit (lib.systems.examples) aarch64-multiplatform-musl;
  } // lib.optionalAttrs (system == "aarch64-linux" && nixpkgsName == "unstable" && !builtins.elem compiler-nix-name ["ghc8107" "ghc902"]) {
    inherit (lib.systems.examples) aarch64-multiplatform-musl;
  };
  isDisabled = d: d.meta.disabled or false;
in
dimension "Nixpkgs version" nixpkgsVersions (nixpkgsName: pinnedNixpkgsSrc:
  let evalPackages = import pinnedNixpkgsSrc (nixpkgsArgs // { system = evalSystem; });
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
          } // pkgs.lib.optionalAttrs runTests {
            inherit (build) tests;
        })
        # GHCJS builds its own template haskell runner.
        // pkgs.lib.optionalAttrs (ifdLevel >= 2 && crossSystemName != "ghcjs")
            pkgs.haskell-nix.iserv-proxy-exes.${compiler-nix-name}
        // pkgs.lib.optionalAttrs (ifdLevel >= 3) {
          hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; inherit evalPackages compiler-nix-name; }).getComponent "exe:hello";
        })
      ))
    )
  )
