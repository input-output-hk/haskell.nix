{ projectConfig }:
{ lib, config, pkgs, haskellLib, ... }: {
  options = {
    packages = lib.mkOption {
      type = lib.types.unspecified;
      default = ps: builtins.attrValues (haskellLib.selectLocalPackages ps);
    };
    components = lib.mkOption {
      type = lib.types.unspecified;
      default = ps: lib.concatMap haskellLib.getAllComponents (config.packages ps);
    };
    additional = lib.mkOption {
      type = lib.types.unspecified;
      default = _: [];
    };
    withHoogle = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };
    withHaddock = lib.mkOption {
      type = lib.types.bool;
      default = config.withHoogle;
    };
    exactDeps = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
    tools = lib.mkOption {
      type = lib.types.attrsOf lib.types.unspecified;
      default = {};
    };
    packageSetupDeps = lib.mkOption {
      type = lib.types.unspecified;
      default = true;
    };
    enableDWARF = lib.mkOption {
      type = lib.types.unspecified;
      default = false;
    };
    crossPlatforms = lib.mkOption {
      type = lib.types.unspecified;
      default = projectConfig.crossPlatforms;
    };
  };
}
