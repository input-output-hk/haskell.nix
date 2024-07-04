# While creating the nix build plan, we take care to create package derivations
# that do not include any reference to the plan itself or how it is created.
#
# This allows haskell.nix to share packages between plans (at least when they
# have identical dependencies). If the package derivations included the hash of
# the plan derivation, different plans would always produce different packages
# and there could not be any sharing of packages between plans.
#
# Any wrangling of the project dependencies (e.g. fetching package indices,
# source-repository-packages or any other asset required for building) *has* to
# be performed during planning. The nix build plan will import any remote asset
# through a fixed-output derivations (i.e. a call to a fetcher).
#
# tl;dr: the builder must not re-introduce any reference to the build plan.

{ pkgs, buildPackages, pkgsBuildBuild, evalPackages, stdenv, lib, haskellLib, ghc, compiler-nix-name, fetchurl, nonReinstallablePkgs, hsPkgs, compiler }:

let
  # Builds a single component of a package.
  comp-builder = haskellLib.weakCallPackage pkgs ./comp-builder.nix {
    inherit ghc haskellLib makeConfigFiles haddockBuilder ghcForComponent hsPkgs compiler nonReinstallablePkgs;
  };

  haddockBuilder = haskellLib.weakCallPackage pkgs ./haddock-builder.nix {
    inherit ghc ghcForComponent haskellLib makeConfigFiles nonReinstallablePkgs;
  };

  setup-builder = haskellLib.weakCallPackage pkgs ./setup-builder.nix {
    ghc = (ghc.passthru.buildGHC or ghc);
    hsPkgs = hsPkgs.buildPackages;
    # We need to use the buildPackages stdenv to build the setup-builder.
    # in the native case, it would be the same in the cross case however
    # we *really* want to build the Setup.hs on the build machine and not
    # have the stdenv confuse it with the target/host env.
    inherit (buildPackages) stdenv;
    inherit buildPackages;
    inherit haskellLib nonReinstallablePkgs makeSetupConfigFiles;
  };

  # Wraps GHC to provide dependencies in a way that works for both the
  # component builder and for nix-shells.
  ghcForComponent = import ./ghc-for-component-wrapper.nix {
    inherit lib ghc haskellLib;
    inherit (buildPackages) stdenv;
    inherit (buildPackages.buildPackages) runCommand makeWrapper;
    inherit (buildPackages.buildPackages.xorg) lndir;
  };

  # Builds a derivation which contains a ghc package-db of
  # dependencies for a component.
  makeConfigFiles = haskellLib.weakCallPackage pkgs ./make-config-files.nix {
    inherit ghc haskellLib nonReinstallablePkgs;
  };
  # When building setup depends we need to use the build systems GHC and Packages
  makeSetupConfigFiles = haskellLib.weakCallPackage buildPackages ./make-config-files.nix {
    inherit haskellLib nonReinstallablePkgs;
    ghc = (ghc.passthru.buildGHC or ghc);
  };


  hoogleLocal = let
    # Use hoogle.nix from at least nixpkgs 22.05
    nixpkgs = if lib.versionAtLeast lib.trivial.release "22.05"
      then pkgs.path
      else pkgs.haskell-nix.sources.nixpkgs-2205;
    nixpkgsHoogle = import (nixpkgs + /pkgs/development/haskell-modules/hoogle.nix);
  in { packages ? [], hoogle ? pkgs.buildPackages.haskell-nix.tool "ghc928" "hoogle" {
        inherit evalPackages;
        version = "5.0.18.3";
        # index-state = pkgs.haskell-nix.internalHackageIndexState;
        index-state = "2023-06-05T00:00:00Z";
      }
    }:
    let
      haskellPackages = {
        # For musl we can use haddock from the buildGHC
        ghc = if stdenv.hostPlatform.isLinux && stdenv.targetPlatform.isMusl && !haskellLib.isNativeMusl
          then ghc.buildGHC
          else ghc;
        inherit packages hoogle;
      };
    in haskellLib.weakCallPackage pkgs nixpkgsHoogle {
      inherit haskellPackages;
    } (p: p.packages);

  # Same as haskellPackages.shellFor in nixpkgs.
  shellFor = haskellLib.weakCallPackage pkgs ./shell-for.nix {
    inherit hsPkgs ghcForComponent makeConfigFiles hoogleLocal haskellLib pkgsBuildBuild evalPackages compiler;
    inherit (buildPackages) glibcLocales;
  };

  # Same as haskellPackages.ghcWithPackages and ghcWithHoogle in nixpkgs.
  withPackages = {withHoogle}: packages: (shellFor {
    name = ghc.name + "-with-packages";
    packages = _: [];
    additional = packages;
    inherit withHoogle;
  }).ghc;

in {
  # Build a Haskell package from its config.
  # TODO: this pkgs is the adjusted pkgs, but pkgs.pkgs is unadjusted
  build-package = haskellLib.weakCallPackage pkgs ./hspkg-builder.nix {
    inherit haskellLib ghc compiler-nix-name comp-builder setup-builder;
  };

  inherit shellFor makeConfigFiles;

  ghcWithPackages = withPackages { withHoogle = false; };
  ghcWithHoogle = withPackages { withHoogle = true; };
}
