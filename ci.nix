# 'supportedSystems' restricts the set of systems that we will evaluate for. Useful when you're evaluting
# on a machine with e.g. no way to build the Darwin IFDs you need! 
{ supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
, ifdLevel ? 3
# Whether or not we are evaluating in restricted mode. This is true in Hydra, but not in Hercules.
, restrictEval ? false }:
let
  inherit (import ./dimension.nix) dimension;
  nixpkgsVersions = {
    "R1903" = "release-19.03";
    "R1909" = "release-19.09";
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
    in lib.optionalAttrs (system == "x86_64-linux" || nixpkgsName == "R1903") {
    # Windows cross compilation is currently broken on macOS for nixpkgs 19.09 (works on 19.03)
    inherit (lib.systems.examples) mingwW64;
  } // lib.optionalAttrs (system == "x86_64-linux") {
    # Musl cross only works on linux
    # aarch64 cross only works on linux
    inherit (lib.systems.examples) musl64 aarch64-multiplatform;
  };
  haskellNixArgs = import ./.;
in
dimension "Nixpkgs version" nixpkgsVersions (nixpkgsName: nixpkgs-pin:
  # We need this for generic nixpkgs stuff at the right version
  let genericPkgs = import ./nixpkgs { inherit nixpkgs-pin; };
  # The compilers contain 'buildPackages', which contains itself. This makes the job collection for Hydra go
  # into an infinite loop
  in genericPkgs.lib.filterAttrsRecursive (n: _: n != "buildPackages") (dimension "System" (systems genericPkgs) (systemName: system:
    let pkgs = import ./nixpkgs (haskellNixArgs // { inherit nixpkgs-pin system; });
        # TODO: can we use meta.platforms for this sort of thing?
        buildBlacklisted = n:
              # update-docs depends on glibc which doesn't build on darwin
              (system == "x86_64-darwin" && (n == "update-docs" || n == "glibc"))
              ||
              # update-hackage accesses the hackage index at eval time (!), which doesn't work in restricted mode
              # https://github.com/input-output-hk/haskell.nix/issues/507
              (restrictEval && (n == "update-hackage")) ;
          build = pkgs.lib.filterAttrsRecursive (n: _: !(buildBlacklisted n)) (import ./build.nix { inherit pkgs ifdLevel; });
    in {
      # Native builds
      # TODO: can we merge this into the general case by picking an appropriate "cross system" to mean native?
      native = pkgs.recurseIntoAttrs {
        inherit (build) tests maintainer-scripts maintainer-script-cache;
        hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; }).components.exes.hello;
        iserv-proxy = pkgs.ghc-extra-packages.ghc865.iserv-proxy.components.exes.iserv-proxy;
        ghc = pkgs.recurseIntoAttrs pkgs.haskell-nix.compiler;
      };
    }
    //
    dimension "Cross system" (crossSystems nixpkgsName genericPkgs system) (crossSystemName: crossSystem:
      # Cross builds
      let pkgs = import ./nixpkgs (haskellNixArgs // { inherit nixpkgs-pin system crossSystem; });
          build = pkgs.lib.filterAttrsRecursive (n: _: !(buildBlacklisted n)) (import ./build.nix { inherit pkgs ifdLevel; });
      in pkgs.recurseIntoAttrs {
        inherit (build) tests;
        hello = (pkgs.haskell-nix.hackage-package { name = "hello"; version = "1.0.0.2"; }).components.exes.hello;
        iserv-proxy = pkgs.ghc-extra-packages.ghc865.iserv-proxy.components.exes.iserv-proxy;
        remote-iserv = pkgs.ghc-extra-packages.ghc865.remote-iserv.components.exes.remote-iserv;
      }
    )
  ))
)
