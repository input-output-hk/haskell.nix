{ pkgs, buildPackages, stdenv, lib, haskellLib, ghc, buildGHC, fetchurl, pkgconfig, nonReinstallablePkgs, hsPkgs }:

let
  # Builds a single component of a package.
  comp-builder = haskellLib.weakCallPackage pkgs ./comp-builder.nix {
    inherit ghc haskellLib makeConfigFiles ghcForComponent hsPkgs;
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

in {
  # Build a Haskell package from its config.
  # TODO: this pkgs is the adjusted pkgs, but pkgs.pkgs is unadjusted
  build-package = haskellLib.weakCallPackage pkgs ./hspkg-builder.nix {
    inherit haskellLib ghc buildGHC comp-builder nonReinstallablePkgs hsPkgs;
  };

  # Same as haskellPackages.shellFor in nixpkgs.
  shellFor = haskellLib.weakCallPackage pkgs ./shell-for.nix {
    inherit hsPkgs ghcForComponent makeConfigFiles;
    inherit (buildPackages) glibcLocales;
  };
}
