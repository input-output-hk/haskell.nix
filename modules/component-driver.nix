{ config, pkgs, lib, haskellLib, buildModules, ... }:
let
  # TODO: this pkgs is the adjusted pkgs, but pkgs.pkgs is unadjusted
  new-builder = haskellLib.weakCallPackage pkgs ../builder {
    inherit haskellLib;
    ghc = config.ghc.package;
    buildGHC = buildModules.config.ghc.package;
     inherit (config) nonReinstallablePkgs hsPkgs;
    inherit withPackage;
  };

  withPackage = import ../builder/with-package-wrapper.nix {
    inherit lib;
    inherit (pkgs.buildPackages) stdenv runCommand makeWrapper;
    inherit (pkgs.buildPackages.xorg) lndir;
    ghc = config.ghc.package;
  };

in

{
  options.nonReinstallablePkgs = lib.mkOption {
    type = lib.types.listOf lib.types.str;
  };

  config.nonReinstallablePkgs =
    [ "rts" "ghc" "ghc-prim" "integer-gmp" "integer-simple" "base"
    "array" "deepseq" "pretty" "ghc-boot-th" "template-haskell" "ghc-heap" ];

  options.hsPkgs = lib.mkOption {
    type = lib.types.unspecified;
  };

  config.hsPkgs = { buildPackages = buildModules.config.hsPkgs; }
    // lib.mapAttrs
      (name: pkg: if pkg == null then null else new-builder pkg)
      (config.packages // lib.genAttrs config.nonReinstallablePkgs (_: null));
}
