{ pkgs, lib, haskellLib, ghc, weakCallPackage, plan }:
let
  new-builder = weakCallPackage pkgs ../builder {
    inherit haskellLib ghc weakCallPackage;
  };
in
lib.fix (self:
  { buildPackages = self; }
  // lib.mapAttrs (_: pkg: if pkg == null then null else new-builder (lib.fix pkg)) (plan.packages self)
)
