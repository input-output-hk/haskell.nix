# This file contains the package set used by the release.nix jobset.
#
# It is separate from default.nix because that file is the public API
# of Haskell.nix, which shouldn't have tests, etc.
{ nixpkgs ? haskellNix.sources.nixpkgs-unstable
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgs nixpkgsArgs
, evalPackages ? import nixpkgs nixpkgsArgs
# This version is used to make our GitHub Action runners happy
# Using `nixpkgs-unstable` currently results in:
#   version `GLIBCXX_3.4.30' not found
, nixpkgsForGitHubAction ? haskellNix.sources.nixpkgs-2211
, pkgsForGitHubAction ? import nixpkgsForGitHubAction (nixpkgsArgs // { inherit (pkgs) system; })
, ifdLevel ? 1000
, compiler-nix-name ? throw "No `compiler-nix-name` passed to build.nix"
, haskellNix ? (import ./default.nix {})
}:

let
  haskell = pkgs.haskell-nix;
  buildHaskell = pkgs.buildPackages.haskell-nix;
  tool = buildHaskell.tool;
in rec {
  tests = import ./test/default.nix { inherit pkgs evalPackages ifdLevel compiler-nix-name; };

  tools = pkgs.lib.optionalAttrs (ifdLevel >= 3) (
    pkgs.recurseIntoAttrs ({
      cabal-latest = tool compiler-nix-name "cabal" { inherit evalPackages; cabalProjectLocal = builtins.readFile ./test/cabal.project.local; };
    } // pkgs.lib.optionalAttrs (__compareVersions haskell.compiler.${compiler-nix-name}.version "9.6" < 0) {
      hlint-latest = tool compiler-nix-name "hlint" {
        inherit evalPackages;
        version = {
            "ghc865" = "3.2.8";
            "ghc882" = "3.3.6";
            "ghc883" = "3.3.6";
            "ghc884" = "3.3.6";
            "ghc8101" = "3.4.1";
            "ghc8102" = "3.4.1";
            "ghc8103" = "3.4.1";
            "ghc8104" = "3.4.1";
            "ghc8105" = "3.4.1";
            "ghc8106" = "3.4.1";
            "ghc8107" = "3.4.1";
          }.${compiler-nix-name} or "latest";
      };
    } // pkgs.lib.optionalAttrs (
             __compareVersions haskell.compiler.${compiler-nix-name}.version "9.2" >= 0
          && __compareVersions haskell.compiler.${compiler-nix-name}.version "9.6" < 0) {
      stack =
        tool compiler-nix-name "stack" {
          version = "2.11.1";
          inherit evalPackages;
        };
    } // pkgs.lib.optionalAttrs (__compareVersions haskell.compiler.${compiler-nix-name}.version "9.0" < 0) {
      # This version will build for ghc < 9.8, but we are only going to test it for
      # ghc < 9.0 (since newer versions do not work with ghc 8.10.7).
      "hls-22" = tool compiler-nix-name "haskell-language-server" {
        inherit evalPackages;
        src = pkgs.haskell-nix.sources."hls-2.2";
      };
    } // pkgs.lib.optionalAttrs (
      __compareVersions haskell.compiler.${compiler-nix-name}.version "9.0" >= 0 &&
      __compareVersions haskell.compiler.${compiler-nix-name}.version "9.8" < 0
    ) {
      "hls-23" = tool compiler-nix-name "haskell-language-server" {
        inherit evalPackages;
        src = pkgs.haskell-nix.sources."hls-2.3";
      };
    } // pkgs.lib.optionalAttrs (
      __compareVersions haskell.compiler.${compiler-nix-name}.version "9.0" >= 0 &&
      __compareVersions haskell.compiler.${compiler-nix-name}.version "9.9" < 0
    ) {
      "hls-24" = tool compiler-nix-name "haskell-language-server" {
        inherit evalPackages;
        src = pkgs.haskell-nix.sources."hls-2.4";
        # Even though this is in the cabal.project it is inside a condional
        # and so haskell.nix cannot parse it properly.  Luckily adding it
        # again seems to work fine.
        cabalProjectLocal = ''
          repository head.hackage.ghc.haskell.org
            url: https://ghc.gitlab.haskell.org/head.hackage/
            secure: True
            key-threshold: 3
            root-keys:
               f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89
               26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329
               7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d
            --sha256: sha256-aVI93DtHziicNn2mGli0YE+bC5BeT7mOQQETp2Thi68=

          if impl(ghc < 9.7)
            active-repositories: hackage.haskell.org
        '';
      };
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
      # Update scripts use the internal nix-tools (compiled with a fixed GHC version)
      nix-tools = haskell.nix-tools-unchecked;
      inherit (haskell) update-index-state-hashes cabal-issue-8352-workaround;
    };
    update-stackage = haskell.callPackage ./scripts/update-stackage.nix {
      inherit (pkgs) stdenv lib writeScript coreutils glibc git
        openssh nixFlakes gawk bash curl findutils;
      # Update scripts use the internal nix-tools (compiled with a fixed GHC version)
      nix-tools = haskell.nix-tools-unchecked;
      inherit (haskell) cabal-issue-8352-workaround;
    };
    update-pins = haskell.callPackage ./scripts/update-pins.nix {};
    update-docs = pkgs.buildPackages.callPackage ./scripts/update-docs.nix {
      generatedOptions = pkgs.callPackage ./scripts/options-doc.nix { };
    };
    check-hydra = pkgs.buildPackages.callPackage ./scripts/check-hydra.nix {};
    check-closure-size = pkgs.buildPackages.callPackage ./scripts/check-closure-size.nix {
      # Includes cabal-install since this is commonly used.
      nix-tools = pkgs.linkFarm "common-tools" [
        { name = "nix-tools";     path = haskell.nix-tools; }
        { name = "cabal-install"; path = haskell.cabal-install.${compiler-nix-name}; }
      ];
    };
    check-materialization-concurrency = pkgs.buildPackages.callPackage ./scripts/check-materialization-concurrency/check.nix {};
    check-path-support = pkgsForGitHubAction.buildPackages.callPackage ./scripts/check-path-support.nix {
      inherit compiler-nix-name;
    };
  };

  # These are pure parts of maintainer-script so they can be built by hydra
  # and added to the cache to speed up buildkite.
  maintainer-script-cache = pkgs.recurseIntoAttrs (
      (pkgs.lib.optionalAttrs (pkgs.system == "x86_64-linux") {
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
