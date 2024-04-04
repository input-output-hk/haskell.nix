{ lib, haskellLib, ... }:

let
  inherit (lib) types;
  inherit (haskellLib.types) listOfFilteringNulls;

in
{
  imports = [
    ./component-options.nix
    ./package-options.nix
  ];

  options = {
    plugins = lib.mkOption {
      type = types.listOf (types.submodule {
        options = {
          library = lib.mkOption {
            type = types.unspecified;
          };

          moduleName = lib.mkOption {
            type = types.str;
          };

          args = lib.mkOption {
            type = types.listOf types.str;
            default = [ ];
          };
        };
      });

      default = [ ];
    };

    depends = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    libs = lib.mkOption {
      type = listOfFilteringNulls (types.either (types.nullOr types.package) (listOfFilteringNulls types.package));
      default = [ ];
    };

    frameworks = lib.mkOption {
      type = listOfFilteringNulls types.package;
      default = [ ];
    };

    pkgconfig = lib.mkOption {
      type = types.listOf (listOfFilteringNulls types.package);
      default = [ ];
    };

    build-tools = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    modules = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    asmSources = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    cmmSources = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    cSources = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    cxxSources = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    jsSources = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    hsSourceDirs = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ "." ];
    };

    includeDirs = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    includes = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    mainPath = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    extraSrcFiles = lib.mkOption {
      type = listOfFilteringNulls types.unspecified;
      default = [ ];
    };

    platforms = lib.mkOption {
      type = types.nullOr (listOfFilteringNulls types.unspecified);
      default = null;
    };
  };
}
