# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluting
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
    "R1909" = "nixpkgs-1909";
    "R2003" = "nixpkgs-2003";
  };
  compilerNixNames = nixpkgsName: nixpkgs: builtins.mapAttrs (defaultCompilerNixName: _:
    (import ./default.nix { inherit checkMaterialization defaultCompilerNixName; }).nixpkgsArgs) ({
    ghc865 = {};
  } // nixpkgs.lib.optionalAttrs (nixpkgsName == "R2003") {
    ghc883 = {};
    ghc8101 = {};
  });
  systems = nixpkgs: nixpkgs.lib.filterAttrs (_: v: builtins.elem v supportedSystems) {
    # I wanted to take these from 'lib.systems.examples', but apparently there isn't one for linux!
    linux = "x86_64-linux";
    darwin = "x86_64-darwin";
  };
  crossSystems = nixpkgsName: nixpkgs: compilerNixName: system:
    # We need to use the actual nixpkgs version we're working with here, since the values
    # of 'lib.systems.examples' are not understood between all versions
    let lib = nixpkgs.lib;
    in lib.optionalAttrs (system == "x86_64-linux" && compilerNixName != "ghc8101") {
    # Windows cross compilation is currently broken on macOS
    inherit (lib.systems.examples) mingwW64;
  } // lib.optionalAttrs (system == "x86_64-linux") {
    # Musl cross only works on linux
    # aarch64 cross only works on linux
    inherit (lib.systems.examples) musl64 aarch64-multiplatform;
  };
in
dimension "Nixpkgs version" nixpkgsVersions (nixpkgsName: nixpkgs-pin:
  let pinnedNixpkgsSrc = sources.${nixpkgs-pin};
      # We need this for generic nixpkgs stuff at the right version
      genericPkgs = import pinnedNixpkgsSrc {};
  in dimension "GHC version" (compilerNixNames nixpkgsName genericPkgs) (compilerNixName: nixpkgsArgs:
    dimension "System" (systems genericPkgs) (systemName: system:
      let pkgs = import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system; });
          build = import ./build.nix { inherit pkgs ifdLevel; };
          platformFilter = platformFilterGeneric pkgs system;
      in filterAttrsOnlyRecursive (_: v: platformFilter v) {
        # Native builds
        # TODO: can we merge this into the general case by picking an appropriate "cross system" to mean native?
        native = pkgs.recurseIntoAttrs ({
          inherit (build) tests tools maintainer-scripts maintainer-script-cache;
          ghc = pkgs.buildPackages.haskell-nix.compiler."${compilerNixName}";
        } // pkgs.lib.optionalAttrs (ifdLevel >= 1) {
          iserv-proxy = pkgs.ghc-extra-packages."${compilerNixName}".iserv-proxy.components.exes.iserv-proxy;
        } // pkgs.lib.optionalAttrs (ifdLevel >= 3) {
          hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; }).components.exes.hello;
        });
      }
      //
      dimension "Cross system" (crossSystems nixpkgsName genericPkgs compilerNixName system) (crossSystemName: crossSystem:
        # Cross builds
        let pkgs = import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system crossSystem; });
            build = import ./build.nix { inherit pkgs ifdLevel; };
        in pkgs.recurseIntoAttrs (pkgs.lib.optionalAttrs (ifdLevel >= 1) {
          ghc = pkgs.buildPackages.haskell-nix.compiler."${compilerNixName}";
          # TODO: look into cross compiling ghc itself
          # ghc = pkgs.haskell-nix.compiler."${compilerNixName}";
          # TODO: look into making tools work when cross compiling
          # inherit (build) tools;
          # Tests are broken on aarch64 cross https://github.com/input-output-hk/haskell.nix/issues/513
          tests =
            if (crossSystemName != "aarch64-multiplatform")
              then build.tests
              else pkgs.recurseIntoAttrs {
                # Even on aarch64 we still want to build the pinned files
                inherit (build.tests) haskellNixRoots;
              };
        } // pkgs.lib.optionalAttrs (ifdLevel >= 2) {
          remote-iserv = pkgs.ghc-extra-packages."${compilerNixName}".remote-iserv.components.exes.remote-iserv;
          iserv-proxy = pkgs.ghc-extra-packages."${compilerNixName}".iserv-proxy.components.exes.iserv-proxy;
        } // pkgs.lib.optionalAttrs (ifdLevel >= 3) {
          hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; }).components.exes.hello;
        })
      )
    )
  )
)
