{ pkgs, buildPackages, stdenv, lib, haskellLib, ghc, fetchurl, pkgconfig, nonReinstallablePkgs, hsPkgs }:

let
  # Builds a single component of a package.
  comp-builder = haskellLib.weakCallPackage pkgs ./comp-builder.nix {
    inherit ghc haskellLib makeConfigFiles ghcForComponent hsPkgs;
  };

  setup-builder = haskellLib.weakCallPackage pkgs ./setup-builder.nix {
    ghc = (ghc.passthru.buildGHC or ghc);
    hsPkgs = hsPkgs.buildPackages;
    # We need to use the buildPackages stdenv to build the setup-builder.
    # in the native case, it would be the same in the corss case however
    # re *really* want to build the Setup.hs on the build machine and not
    # have the stdenv confuse it with the target/host env.
    inherit (buildPackages) stdenv;
    inherit buildPackages pkgconfig;
    inherit haskellLib nonReinstallablePkgs makeSetupConfigFiles;
  };

  # Wraps GHC to provide dependencies in a way that works for both the
  # component builder and for nix-shells.
  ghcForComponent = import ./ghc-for-component-wrapper.nix {
    inherit lib ghc;
    inherit (buildPackages) stdenv runCommand makeWrapper;
    inherit (buildPackages.xorg) lndir;
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
    # Use the latest default nixpkgs hoogle.nix, as the 19.03 one does not work with cross compilers
    nixpkgsHoogleLocal = import ((import (import ../nixpkgs).nixpkgs-default {}).path + /pkgs/development/haskell-modules/hoogle.nix);
  in { packages ? [], hoogle ? pkgs.buildPackages.haskell-nix.haskellPackages.hoogle.components.exes.hoogle }:
    haskellLib.weakCallPackage pkgs nixpkgsHoogleLocal {
      # For musl we can use haddock from the buildGHC
      ghc = if stdenv.hostPlatform.isLinux && stdenv.targetPlatform.isMusl
        then ghc.buildGHC
        else ghc;
      inherit packages hoogle;
    };

  # Same as haskellPackages.shellFor in nixpkgs.
  shellFor = haskellLib.weakCallPackage pkgs ./shell-for.nix {
    inherit hsPkgs ghcForComponent makeConfigFiles hoogleLocal haskellLib;
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
    inherit haskellLib ghc comp-builder setup-builder;
  };

  inherit shellFor;

  ghcWithPackages = withPackages { withHoogle = false; };
  ghcWithHoogle = withPackages { withHoogle = true; };
}
