{ lib, config, pkgs, ... }:

with lib;

############################################################################
# this is an attempt to use mkMerge for combining package options
############################################################################
let
  haskellLib = let hl = import ../lib { inherit lib; haskellLib = hl; }; in hl;

  subComponents = packageConfig: sub: attrValues (packageConfig.components.${sub} or {});

  allComponent = packageConfig: {
    components.all = mkMerge ([ packageConfig.components.library ] ++
        concatMap (subComponents packageConfig) haskellLib.subComponentTypes);
  };

in {
  # Does not work -- infinite recursion
  # packages = mapAttrs (name: packageConfig: allComponent packageConfig) config.packages;

  # A single package works
  # packages.cabal-simple = allComponent (config.packages.cabal-simple);
}
