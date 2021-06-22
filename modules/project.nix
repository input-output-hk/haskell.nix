{ lib, ... }: {
  _file = "haskell.nix/modules/project.nix";
  options = {
    projectFileName = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
    };
  };
}