# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluting
# on a machine with e.g. no way to build the Darwin IFDs you need! 
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 3
# Whether or not we are evaluating in restricted mode. This is true in Hydra, but not in Hercules.
, restrictEval ? false }:
let
  inherit (import ./ci-lib.nix) dimension platformFilterGeneric filterAttrsOnlyRecursive;
  inherit (import ./default.nix { checkMaterialization = false; }) sources nixpkgsArgs;
  nixpkgsVersions = {
    "R1909" = "nixpkgs-1909";
    "R2003" = "nixpkgs-2003";
  };
  systems = nixpkgs: nixpkgs.lib.filterAttrs (_: v: builtins.elem v supportedSystems) {
    # I wanted to take these from 'lib.systems.examples', but apparently there isn't one for linux!
    linux = "x86_64-linux";
    darwin = "x86_64-darwin";
  };
  crossSystems = nixpkgsName: nixpkgs: system:
    # We need to use the actual nixpkgs version we're working with here, since the values
    # of 'lib.systems.examples' are not understood between all versions
    let lib = nixpkgs.lib;
    in lib.optionalAttrs (system == "x86_64-linux") {
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
  in dimension "System" (systems genericPkgs) (systemName: system:
    let pkgs = import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system; });
        build = import ./build.nix { inherit pkgs ifdLevel; };
        platformFilter = platformFilterGeneric pkgs system;
        compilers = {
          inherit (pkgs.haskell-nix.compiler) ghc865 ghc883;
        };
    in filterAttrsOnlyRecursive (_: v: platformFilter v) {
      # Native builds
      # TODO: can we merge this into the general case by picking an appropriate "cross system" to mean native?
      native = pkgs.recurseIntoAttrs ({
        inherit (build) tests tools maintainer-scripts maintainer-script-cache;
        ghc = pkgs.recurseIntoAttrs compilers;
      } // pkgs.lib.optionalAttrs (ifdLevel >= 1) {
        iserv-proxy = pkgs.recurseIntoAttrs (
          pkgs.lib.mapAttrs (ghcName: _:
            pkgs.ghc-extra-packages."${ghcName}".iserv-proxy.components.exes.iserv-proxy
          ) compilers);
      } // pkgs.lib.optionalAttrs (ifdLevel >= 2) {
        hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; }).components.exes.hello;
      });
    }
    //
    dimension "Cross system" (crossSystems nixpkgsName genericPkgs system) (crossSystemName: crossSystem:
      # Cross builds
      let pkgs = import pinnedNixpkgsSrc (nixpkgsArgs // { inherit system crossSystem; });
          build = import ./build.nix { inherit pkgs ifdLevel; };
      in pkgs.recurseIntoAttrs (pkgs.lib.optionalAttrs (ifdLevel >= 1) {
        ghc = pkgs.recurseIntoAttrs compilers;
        # TODO: look into making tools work when cross compiling
        # inherit (build) tools;
      } // pkgs.lib.optionalAttrs (ifdLevel >= 2) {
        remote-iserv = pkgs.recurseIntoAttrs (
          pkgs.lib.mapAttrs (ghcName: _:
            pkgs.ghc-extra-packages."${ghcName}".remote-iserv.components.exes.remote-iserv
          ) compilers);
        iserv-proxy = pkgs.recurseIntoAttrs (
          pkgs.lib.mapAttrs (ghcName: _:
            pkgs.ghc-extra-packages."${ghcName}".iserv-proxy.components.exes.iserv-proxy
          ) compilers);
      } // pkgs.lib.optionalAttrs (ifdLevel >= 3) {
        hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; }).components.exes.hello;
      }
      //
      # Tests are broken on aarch64 cross https://github.com/input-output-hk/haskell.nix/issues/513
      pkgs.lib.optionalAttrs (crossSystemName != "aarch64-multiplatform") {
        inherit (build) tests;
      })
    )
  )
)
