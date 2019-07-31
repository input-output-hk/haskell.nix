{ pkgs, buildPackages, stdenv, lib, haskellLib, ghc, buildGHC, fetchurl, pkgconfig, nonReinstallablePkgs, hsPkgs }:

let
  # Builds a single component of a package.
  comp-builder = haskellLib.weakCallPackage pkgs ./comp-builder.nix {
    inherit ghc haskellLib makeConfigFiles ghcForComponent hsPkgs;
  };

  setup-builder = haskellLib.weakCallPackage pkgs ./setup-builder.nix {
    ghc = buildGHC;
    hsPkgs = hsPkgs.buildPackages;
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
    ghc = buildGHC;
  };


  hoogleLocal = let
    nixpkgsHoogleLocal = import (pkgs.path + /pkgs/development/haskell-modules/hoogle.nix);
  in { packages ? [], hoogle ? pkgs.haskellPackages.hoogle }:
    haskellLib.weakCallPackage pkgs nixpkgsHoogleLocal {
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
    inherit haskellLib ghc buildGHC comp-builder setup-builder makeSetupConfigFiles;
  };

  inherit shellFor;

  ghcWithPackages = withPackages { withHoogle = false; };
  ghcWithHoogle = withPackages { withHoogle = true; };
}
