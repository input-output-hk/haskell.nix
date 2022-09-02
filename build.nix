# This file contains the package set used by the release.nix jobset.
#
# It is separate from default.nix because that file is the public API
# of Haskell.nix, which shouldn't have tests, etc.
let
  haskellNix = (import ./default.nix {});
in
{ nixpkgs ? haskellNix.sources.nixpkgs-2205
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgs nixpkgsArgs
, evalPackages ? import nixpkgs nixpkgsArgs
, nixpkgsForHydra ? haskellNix.sources.nixpkgs-2105
, pkgsForHydra ? import nixpkgsForHydra (nixpkgsArgs // { inherit (pkgs) system; })
, ifdLevel ? 1000
, compiler-nix-name ? throw "No `compiler-nix-name` passed to build.nix"
}:

let
  haskell = pkgs.haskell-nix;
  buildHaskell = pkgs.buildPackages.haskell-nix;
  tool = buildHaskell.tool;
in rec {
  tests = import ./test/default.nix { inherit pkgs evalPackages ifdLevel compiler-nix-name; };

  tools = pkgs.lib.optionalAttrs (ifdLevel >= 3) (
    pkgs.recurseIntoAttrs ({
      cabal-latest = tool compiler-nix-name "cabal" { inherit evalPackages; };
      hlint-latest = tool compiler-nix-name "hlint" {
        inherit evalPackages;
        version = {
            "ghc865" = "3.2.8";
            "ghc882" = "3.3.6";
            "ghc883" = "3.3.6";
            "ghc884" = "3.3.6";
          }.compiler-nix-name or "latest";
      };
      hls-latest = tool compiler-nix-name "haskell-language-server" { inherit evalPackages; };
    })
  );

  # Scripts for keeping Hackage and Stackage up to date, and CI tasks.
  # The dontRecurseIntoAttrs prevents these from building on hydra
  # as not all of them can work in restricted eval mode (as they
  # are not pure).
  maintainer-scripts = pkgs.dontRecurseIntoAttrs {
    update-hackage = import ./scripts/update-hackage.nix {
      inherit (pkgs) stdenv lib writeScript coreutils glibc git
        openssh nixFlakes gawk bash curl findutils;
      # Update scripts use the internal nix-tools and cabal-install (compiled with a fixed GHC version)
      nix-tools = haskell.internal-nix-tools;
      cabal-install = haskell.internal-cabal-install;
      inherit (haskell) update-index-state-hashes;
    };
    update-stackage = haskell.callPackage ./scripts/update-stackage.nix {
      inherit (pkgs) stdenv lib writeScript coreutils glibc git
        openssh nixFlakes gawk bash curl findutils;
      # Update scripts use the internal nix-tools and cabal-install (compiled with a fixed GHC version)
      nix-tools = haskell.internal-nix-tools;
      cabal-install = haskell.internal-cabal-install;
    };
    update-pins = haskell.callPackage ./scripts/update-pins.nix {};
    update-docs = pkgs.buildPackages.callPackage ./scripts/update-docs.nix {
      generatedOptions = pkgs.callPackage ./scripts/options-doc.nix { };
    };
    # Because this is going to be used to test caching on hydra, it must not
    # use the darcs package from the haskell.nix we are testing.  For that reason
    # it uses `pkgs.buildPackages.callPackage` not `haskell.callPackage`
    # (We could pull in darcs from a known good haskell.nix for hydra to
    # use)
    check-hydra = pkgsForHydra.buildPackages.callPackage ./scripts/check-hydra.nix {};
    check-closure-size = pkgs.buildPackages.callPackage ./scripts/check-closure-size.nix {
      # Includes cabal-install since this is commonly used.
      nix-tools = pkgs.linkFarm "common-tools" [
        { name = "nix-tools";     path = haskell.nix-tools.${compiler-nix-name}; }
        { name = "cabal-install"; path = haskell.cabal-install.${compiler-nix-name}; }
      ];
    };
    check-materialization-concurrency = pkgs.buildPackages.callPackage ./scripts/check-materialization-concurrency/check.nix {};
    # Forcing nixpkgs-unstable here because this test makes a script
    # that when run will build `aeson` (used by `tests/cabal-simple`)
    # and we currently do not build that on hydra for nixpkgs-2205 (used by `pkgs`).
    # Using nixpkgs-unstable should allow buildkite to find what it needs
    # in the hydra cache when it runs the script.
    check-path-support = (import haskellNix.sources.nixpkgs-unstable nixpkgsArgs)
        .buildPackages.callPackage ./scripts/check-path-support.nix {
      inherit compiler-nix-name;
    };
  };

  # These are pure parts of maintainer-script so they can be built by hydra
  # and added to the cache to speed up buildkite.
  maintainer-script-cache = pkgs.recurseIntoAttrs (
      (pkgs.lib.optionalAttrs (pkgsForHydra.system == "x86_64-linux") {
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
