{ config, pkgs, lib, haskellLib, ... }:
let
  # TODO: this pkgs is the adjusted pkgs, but pkgs.pkgs is unadjusted
  new-builder = haskellLib.weakCallPackage pkgs ../builder {
    inherit haskellLib;
    ghc = config.ghc.package;
  };
in

{
  options.hsPkgs = lib.mkOption {
    type = lib.types.unspecified;
  };

  config.hsPkgs = { buildPackages = config.hsPkgs; }
    // lib.mapAttrs (_: _: null) config.compiler.packages
    // lib.mapAttrs (_: pkg: if pkg == null then null else new-builder pkg) config.packages;
}
