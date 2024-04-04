{ lib, haskellLib, ... }:
{
  options = {
    preUnpack = lib.mkOption {
      type = lib.types.nullOr lib.types.lines;
      default = null;
    };

    postUnpack = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    prePatch = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    postPatch = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    preConfigure = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    postConfigure = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    preBuild = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    postBuild = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    preCheck = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    # Wrapper for test executable run in checkPhase
    testWrapper = lib.mkOption {
      type = haskellLib.types.listOfFilteringNulls lib.types.str;
      default = [ ];
      description = "A command to run for executing tests in checkPhase, which takes the original test command as its arguments.";
      example = "echo";
    };

    postCheck = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    preInstall = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    postInstall = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    preHaddock = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    postHaddock = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = null;
    };

    hardeningDisable = lib.mkOption {
      type = haskellLib.types.listOfFilteringNulls lib.types.str;
      default = [ ];
    };

    ghcOptions = lib.mkOption {
      type = haskellLib.types.listOfFilteringNulls lib.types.str;
      default = [ ];
    };

    contentAddressed = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = ''
        Build content addressed derivation, requires Nix to have experimental feature
        `ca-derivations` enabled.
      '';
    };

    planned = lib.mkOption {
      description = "Set to true by `plan-to-nix` for any component that was included in the `plan.json` file.";
      # This is here so that (rather than in componentOptions) so it can be set project wide for stack projects
      type = lib.types.bool;
      default = false;
    };
  };
}
