{ config, pkgs, lib, haskellLib, buildModules, ... }:
let
  # TODO: this pkgs is the adjusted pkgs, but pkgs.pkgs is unadjusted
  new-builder = haskellLib.weakCallPackage pkgs ../builder {
    inherit haskellLib;
    ghc = config.ghc.package;
    buildGHC = buildModules.config.ghc.package;
    inherit (config) nonReinstallablePkgs hsPkgs;
    inherit ghcForComponent;
  };

  ghcForComponent = import ../builder/ghc-for-component-wrapper.nix {
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

  # Dependencies
  # 
  #              .--------.           .------------------.
  #              | pretty | < ------- | template-haskell |
  #              '--------'           '------------------'
  #                   v                          |
  #              .---------.     .-------.       |
  #              | deepseq | - > | array |       |     
  #              '---------'     '-------'       v 
  #                    v            v         .-------------.
  # .----------.  .----------.  .------.   .- | ghc-boot-th |
  # | ghc-heap |  | ghc-prim |  | base |< -'  '-------------'
  # '----------'  '----------'  '------'  .----------------.
  #       |          v           |  |     | integer-simple |
  #       |       .-----.        |  '-- > |-------or-------|
  #       '---- > | rts | < -----'        | integer-gmp    |
  #               '-----'                 '----------------'

  config.nonReinstallablePkgs =
    [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell" ];

  options.hsPkgs = lib.mkOption {
    type = lib.types.unspecified;
  };

  config.hsPkgs = { buildPackages = buildModules.config.hsPkgs; }
    // lib.mapAttrs
      (name: pkg: if pkg == null then null else new-builder pkg)
      (config.packages // lib.genAttrs config.nonReinstallablePkgs (_: null));
}
