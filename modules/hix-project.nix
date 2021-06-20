{ lib, ... }: {
  _file = "haskell.nix/modules/hix-project.nix";
  options = {
    # These are options that only the Hix command wrappers use. If you make a flake
    # with `haskell-nix.hix.project`, then they are ignored.
    haskellNix = lib.mkOption {
      type = lib.types.unspecified;
      default = null;
    };
    nixpkgsPin = lib.mkOption {
      type = lib.types.str;
      default = "nixpkgs-unstable";
    };
    nixpkgs = lib.mkOption {
      type = lib.types.unspecified;
      default = null;
    };
    nixpkgsArgs = lib.mkOption {
      type = lib.types.unspecified;
      default = null;
    };
    overlays = lib.mkOption {
      type = lib.types.unspecified;
      default = [];
    };
    pkgs = lib.mkOption {
      type = lib.types.unspecified;
      default = null;
    };
    project = lib.mkOption {
      type = lib.types.unspecified;
      default = null;
    };
  };

  # Default values for other project options (things that do not have defaults for non hix projects)
  config = {
    compiler-nix-name = lib.mkDefault "ghc8105";
  };
}