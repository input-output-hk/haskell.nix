# This file contains the package set used by the release.nix jobset.
#
# It is separate from default.nix because that file is the public API
# of Haskell.nix, which shouldn't have tests, etc.

{ nixpkgs ? (import ./nixpkgs/default.nix)
, nixpkgsArgs ? (import ./default.nix)
, pkgs ? (nixpkgs nixpkgsArgs)
, ifdLevel ? 1000
}:

let
  haskell = pkgs.haskell-nix;
in rec {
  tests = import ./test/default.nix { inherit pkgs ifdLevel; };

  # Scripts for keeping Hackage and Stackage up to date, and CI tasks.
  # The dontRecurseIntoAttrs prevents these from building on hydra
  # as not all of them can work in restricted eval mode (as they
  # are not pure).
  maintainer-scripts = pkgs.dontRecurseIntoAttrs {
    update-hackage = haskell.callPackage ./scripts/update-hackage.nix {};
    update-stackage = haskell.callPackage ./scripts/update-stackage.nix {};
    update-pins = haskell.callPackage ./scripts/update-pins.nix {};
    update-docs = pkgs.buildPackages.callPackage ./scripts/update-docs.nix {
      generatedOptions = import ./scripts/options-doc.nix {
        # nixpkgs 19.09 changes "Option has no description" from an
        # error into a warning. That is quite helpful when hardly any
        # of our options are documented, thanks @oxij.
        pkgs = import ./nixpkgs { nixpkgs-pin = "release-19.09"; };
      };
    };
    # Because this is going to be used to test caching on hydra, it must not
    # use the darcs package from the haskell.nix we are testing.  For that reason
    # it uses `pkgs.buildPackages.callPackage` not `haskell.callPackage`
    # (We could pull in darcs from a known good haskell.nix for hydra to
    # use)
    check-hydra = pkgs.buildPackages.callPackage ./scripts/check-hydra.nix {};
    check-closure-size = pkgs.buildPackages.callPackage ./scripts/check-closure-size.nix {
      inherit (haskell) nix-tools;
    };
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
