{ lib, ... }: {
  _file = "haskell.nix/modules/hix-project.nix";
  options = {
    # These are options that only the Hix command wrappers use. If you make a flake
    # with `haskell-nix.hix.project`, then they are ignored.
    haskellNix = lib.mkOption {
      type = lib.types.unspecified;
      default = null;
      description = "Imported haskell.nix itself (this will be set by `hix`)";
    };
    nixpkgsPin = lib.mkOption {
      type = lib.types.str;
      default = "nixpkgs-unstable";
      description = "The name of a haskell.nix nixpkgs pin to use (e.g. nixpkgs-unstable or nixpkgs-2111)";
    };
    nixpkgs = lib.mkOption {
      type = lib.types.nullOr lib.types.path;
      default = null;
      description = "Path to the nixpkgs files to use (uses nixpkgsPin by default)";
    };
    nixpkgsArgs = lib.mkOption {
      type = lib.types.unspecified;
      default = null;
      description = "Path to the nixpkgs files to use (this will be set by `hix`)";
    };
    overlays = lib.mkOption {
      type = lib.types.unspecified;
      default = [];
      description = "Extra overlays to use (in addition to those haskell.nix includes)";
    };
    pkgs = lib.mkOption {
      type = lib.types.unspecified;
      default = null;
      description = "The pkgs (this will be set by `hix`)";
    };
    project = lib.mkOption {
      type = lib.types.unspecified;
      default = null;
      description = "The project (this will be set by `hix`)";
    };
  };

  # Default values for other project options (things that do not have defaults for non hix projects)
  config = {
    compiler-nix-name = lib.mkDefault "ghc8107";
  };
}
