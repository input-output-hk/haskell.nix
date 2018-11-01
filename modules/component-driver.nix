{ config, pkgs, lib, haskellLib, ... }:
let
  # TODO: this pkgs is the adjusted pkgs, but pkgs.pkgs is unadjusted
  new-builder = haskellLib.weakCallPackage pkgs ../builder {
    inherit haskellLib;
    ghc = config.ghc.package;
  };

  nonReinstallablePkgs = [ "rts" "ghc" "ghc-prim" "integer-gmp" "integer-simple" "base"
                           "array" "deepseq" "pretty" "ghc-boot-th" "template-haskell"];
in

{
  options.hsPkgs = lib.mkOption {
    type = lib.types.unspecified;
  };

  config.hsPkgs = { buildPackages = config.hsPkgs; }
    // lib.mapAttrs
      (name: pkg: if pkg == null || builtins.elem name nonReinstallablePkgs then null else new-builder pkg)
      config.packages;
}
