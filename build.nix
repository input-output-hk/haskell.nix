# This file contains the package set used by the release.nix jobset.
#
# It is separate from default.nix because that file is the public API
# of Haskell.nix, which shouldn't have tests, etc.
let
  haskellNix = (import ./default.nix {});
in
{ nixpkgs ? haskellNix.sources.nixpkgs-default
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgs nixpkgsArgs
, ifdLevel ? 1000
}:

let
  haskell = pkgs.haskell-nix;
  buildHaskell = pkgs.buildPackages.haskell-nix;
  tool = buildHaskell.tool;
in rec {
  tests = import ./test/default.nix { inherit pkgs ifdLevel; };

  tools = pkgs.lib.optionalAttrs (ifdLevel >= 3) (
    pkgs.recurseIntoAttrs {
      ghcide-020 = tool "ghcide" "0.2.0";
    } // pkgs.lib.optionalAttrs (buildHaskell.defaultCompilerNixName == "ghc865") {
      cabal-30 = tool "cabal" "3.0.0.0";
    } // pkgs.lib.optionalAttrs (buildHaskell.defaultCompilerNixName != "ghc8101") {
      cabal-32 = tool "cabal" "3.2.0.0";
      ghcide-object-code = tool "ghcide" "object-code";
    }
  );

  # Scripts for keeping Hackage and Stackage up to date, and CI tasks.
  # The dontRecurseIntoAttrs prevents these from building on hydra
  # as not all of them can work in restricted eval mode (as they
  # are not pure).
  maintainer-scripts = pkgs.dontRecurseIntoAttrs {
    update-hackage = haskell.callPackage ./scripts/update-hackage.nix {};
    update-stackage = haskell.callPackage ./scripts/update-stackage.nix {};
    update-pins = haskell.callPackage ./scripts/update-pins.nix {};
    update-docs = pkgs.buildPackages.callPackage ./scripts/update-docs.nix {
      generatedOptions = pkgs.callPackage ./scripts/options-doc.nix { };
    };
    # Because this is going to be used to test caching on hydra, it must not
    # use the darcs package from the haskell.nix we are testing.  For that reason
    # it uses `pkgs.buildPackages.callPackage` not `haskell.callPackage`
    # (We could pull in darcs from a known good haskell.nix for hydra to
    # use)
    check-hydra = pkgs.buildPackages.callPackage ./scripts/check-hydra.nix {};
    check-closure-size = pkgs.buildPackages.callPackage ./scripts/check-closure-size.nix {
      # Includes cabal-install and hpack since these are commonly used.
      nix-tools = pkgs.linkFarm "common-tools" [
        { name = "nix-tools";     path = haskell.nix-tools; }
        { name = "cabal-install"; path = haskell.cabal-install; }
        { name = "hpack";         path = haskell.haskellPackages.hpack.components.exes.hpack; }
      ];
    };
    check-materialization-concurrency = pkgs.buildPackages.callPackage ./scripts/check-materialization-concurrency/check.nix {};
  };

  # These are pure parts of maintainer-script so they can be built by hydra
  # and added to the cache to speed up buildkite.
  maintainer-script-cache = pkgs.recurseIntoAttrs (
      (pkgs.lib.optionalAttrs pkgs.stdenv.hostPlatform.isWindows {
        inherit (maintainer-scripts) check-hydra;
      })
    // (pkgs.lib.optionalAttrs (ifdLevel > 2) {
        inherit (maintainer-scripts) update-docs check-closure-size;
        # Some of the dependencies of the impure scripts so that they will
        # will be in the cache too for buildkite.
        inherit (pkgs.buildPackages) glibc coreutils git openssh cabal-install nix-prefetch-git;
        inherit (haskell) nix-tools;
      })
  );
}
