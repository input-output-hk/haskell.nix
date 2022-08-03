{ lib, config, pkgs, ... }:
with lib;
with lib.types;
{
  _file = "haskell.nix/modules/project-common.nix";
  options = {
    # Used by callCabalProjectToNix and callStackToNix
    name = mkOption {
      type = nullOr str;
      default = "haskell-project"; # TODO figure out why `config.src.name or null` breaks hix;
      description = "Optional name for better error messages";
    };
    src = mkOption {
      type = either path package;
    };
    # Default shell arguments
    shell = mkOption {
      # TODO make this a submodule
      type = unspecified;
      default = {};
      description = ''
        Arguments to use for the default shell `p.shell` (these are passed to p.shellFor).
        For instance to include `cabal` and `ghcjs` support use
          shell = { tools.cabal = {}; crossPlatforms = p: [ p.ghcjs ]; }
      '';
    };
    evalSystem = mkOption {
      type = str;
      default = pkgs.buildPackages.system;
      description = ''
        Specifies the system on which `cabal` and `nix-tools` should run.
        If not specified the `buildPackages` system will be used.
        If there are no builders for the `buildPackages` system
        specifying a system for which there are builders will
        allow the evaluation of the haskell project to work.
      '';
    };
    evalPackages = mkOption {
      type = attrs;
      default =
        if pkgs.buildPackages.system == config.evalSystem
          then pkgs.buildPackages
        else
          import pkgs.path {
            system = config.evalSystem;
            overlays = pkgs.overlays;
          };
      description = ''
        Packages used to run `cabal` and `nix-tools`.
        This will default to `pkgs.buildPackages` if it
        matches the `evalSystem` (or if `evalSystem` was
        not specified).
        If a different `evalSystem` was requested, `evalPackages` will
        default to be:
          import pkgs.path {
            system = config.evalSystem;
            overlays = pkgs.overlays;
          };
      '';
    };
    hsPkgs = lib.mkOption {
      type = lib.types.unspecified;
    };
  };
}
