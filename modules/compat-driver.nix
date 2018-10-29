{ lib, config, pkgs, haskellLib, ... }:

{
  options.hsPkgs = lib.mkOption {
    type = lib.types.unspecified;
  };

  config.hsPkgs = pkgs.callPackage "${pkgs.path}/pkgs/development/haskell-modules" {
    haskellLib = pkgs.haskell.lib;
    ghc = config.ghc.package;
    buildHaskellPackages = config.hsPkgs;
    compilerConfig = _: _: lib.mapAttrs (_: _: null) (builtins.removeAttrs config.compiler.packages ["ghc"]);
    initialPackages = args: self:
      lib.mapAttrs
        (_: pkg: self.callPackage ../compat/driver.nix { expr = pkg; inherit lib; })
        config.packages;
    packageSetConfig = config.overlay pkgs;
  };
}
