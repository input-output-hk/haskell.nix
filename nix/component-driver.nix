{ pkgs, lib, haskellLib, ghc, weakCallPackage, plan }:
let
  new-builder = weakCallPackage pkgs ../builder {
    inherit haskellLib ghc weakCallPackage;
  };
in
lib.fix (self:
  { buildPackages = self; }
  // lib.mapAttrs (_: _: null) plan.compiler.packages
  // lib.mapAttrs (_: pkg: new-builder (lib.fix pkg)) (plan.packages self)
)
