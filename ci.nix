# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluating
# on a machine with e.g. no way to build the Darwin IFDs you need!
{ ifdLevel # This is passed in from flake.nix
, checkMaterialization ? false
, system ? builtins.currentSystem
, evalSystem ? "aarch64-darwin"
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
    "R2511" = inputs.nixpkgs-2511;
    "unstable" = inputs.nixpkgs-unstable;
  };

  nixpkgsArgs = {
    # set checkMaterialization as per top-level argument
    overlays = [
      haskellNix.overlay
      (final: prev: {
        haskell-nix = prev.haskell-nix // {
          inherit checkMaterialization;
        };
      })
      (import ./test/overlay.nix)
    ];
    # Needed for dwarf tests
    config = haskellNix.config // {
      permittedInsecurePackages = [
        "libdwarf-20210528"
        "libdwarf-20181024"
        "dwarfdump-20181024"
      ];
      allowUnfreePredicate = pkg: builtins.elem (lib.getName pkg) [
        "platform-tools"
        "ndk"
        "android-sdk-ndk"
        "android-sdk-platform-tools"
        "aarch64-unknown-linux-android-ndk-toolchain-wrapper"
        "aarch64-unknown-linux-android-ndk-toolchain"
        "armv7a-unknown-linux-androideabi-ndk-toolchain-wrapper"
        "armv7a-unknown-linux-androideabi-ndk-toolchain"
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
      nixpkgs.lib.optionalAttrs (builtins.elem nixpkgsName ["R2411" "R2505" "R2511"]) {
        ghc96 = false;
        ghc98 = false;
        ghc910 = false;
        ghc912 = false;
      } // nixpkgs.lib.optionalAttrs (nixpkgsName == "unstable") {
        ghc96 = true;
        ghc98 = true;
        ghc910 = true;
        ghc912 = true;
        ghc912llvm = true;
        ghc914X = true;
        ghc915 = true;
      })));
  crossSystems = nixpkgsName: nixpkgs: compiler-nix-name:
    # We need to use the actual nixpkgs version we're working with here, since the values
    # of 'lib.systems.examples' are not understood between all versions
    let lib = nixpkgs.lib;
    in lib.optionalAttrs (nixpkgsName == "unstable"
      && __match ".*llvm" compiler-nix-name == null
      && builtins.elem system ["aarch64-linux" "x86_64-linux"]) {
        static = p: p.pkgsStatic;
      } // lib.optionalAttrs (nixpkgsName == "unstable"
          && (__match ".*llvm" compiler-nix-name == null)
          && !builtins.elem compiler-nix-name ["ghc9102" "ghc9103"]) {
        inherit (lib.systems.examples) ghcjs;
      } // lib.optionalAttrs (nixpkgsName == "unstable"
          && (__match ".*llvm" compiler-nix-name == null)
          && !builtins.elem compiler-nix-name ["ghc967" "ghc984" "ghc9103"]
          && system != "x86_64-darwin") {
        inherit (lib.systems.examples) wasi32;
      } // lib.optionalAttrs (nixpkgsName == "unstable"
          && (__match ".*llvm" compiler-nix-name == null)
          && ((system == "x86_64-linux"  && !builtins.elem compiler-nix-name ["ghc902" "ghc928"])
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
      } // lib.optionalAttrs (__match ".*llvm" compiler-nix-name == null && system == "x86_64-linux" && nixpkgsName == "unstable" && !builtins.elem compiler-nix-name ["ghc902" "ghc928" "ghc948"]) {
        # Out llvm versions of GHC seem to break for musl32
        inherit (lib.systems.examples) musl32;
      } // lib.optionalAttrs (system == "x86_64-linux"
          && !builtins.elem compiler-nix-name ["ghc967" "ghc984" "ghc9103"]) {
        inherit (lib.systems.examples) aarch64-android-prebuilt;
      } // lib.optionalAttrs (system == "x86_64-linux"
          && nixpkgsName != "unstable"
          && !builtins.elem compiler-nix-name ["ghc967" "ghc984" "ghc9103" "ghc91320250523"]) {
        inherit (lib.systems.examples) armv7a-android-prebuilt;
      } // lib.optionalAttrs (system == "x86_64-linux" && nixpkgsName == "unstable" && !builtins.elem compiler-nix-name ["ghc8107" "ghc902"]) {
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
        native = pkgs.lib.recurseIntoAttrs ({
          roots = pkgs.haskell-nix.roots' { inherit compiler-nix-name evalPackages; } ifdLevel;
        } // pkgs.lib.optionalAttrs runTests {
          inherit (build) tests tools maintainer-scripts maintainer-script-cache;
        } // pkgs.lib.optionalAttrs (ifdLevel >= 3) rec {
          hello = (pkgs.haskell-nix.hackage-package ({ name = "hello"; version = "1.0.0.2"; inherit evalPackages compiler-nix-name; }
            // lib.optionalAttrs (builtins.compareVersions pkgs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.13" >= 0) {
              shell.tools.hoogle.cabalProjectLocal = ''
                allow-newer: hashable:ghc-bignum, integer-logarithms:ghc-bignum
              '';
          })).getComponent "exe:hello";
          # Make sure the default shell tools (hoogle) are built
          simple-shell = (hello.project.flake {}).devShells.default;
        });
      }
      //
      dimension "Cross system" (crossSystems nixpkgsName evalPackages compiler-nix-name) (crossSystemName: crossSystem:
        let pkgs =
              if builtins.isAttrs crossSystem
                then import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system crossSystem; })
                else crossSystem (import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system; }));
            build = import ./build.nix { inherit pkgs evalPackages ifdLevel compiler-nix-name haskellNix; };
        in pkgs.lib.recurseIntoAttrs (pkgs.lib.optionalAttrs (ifdLevel >= 1) ({
            roots = pkgs.haskell-nix.roots' { inherit compiler-nix-name evalPackages; } ifdLevel // {
              ghc = pkgs.buildPackages.haskell-nix.compiler.${compiler-nix-name}.override { ghcEvalPackages = evalPackages; };
            };
            # TODO: look into cross compiling ghc itself
            # ghc = pkgs.haskell-nix.compiler.${compiler-nix-name};
            # TODO: look into making tools work when cross compiling
            # inherit (build) tools;
          } // pkgs.lib.optionalAttrs runTests {
            inherit (build) tests;
        })
        # GHCJS builds its own template haskell runner.
        // pkgs.lib.optionalAttrs (ifdLevel >= 2 && !builtins.elem crossSystemName ["ghcjs" "wasi32"])
            pkgs.haskell-nix.iserv-proxy-exes.${compiler-nix-name}
        // pkgs.lib.optionalAttrs (ifdLevel >= 3) {
          hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; inherit evalPackages compiler-nix-name; }).getComponent "exe:hello";
        })
      ))
    )
  )
