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
      default = builtins.currentSystem or pkgs.buildPackages.system;
      description = ''
        Specifies the system on which `cabal configure` and `plan-to-nix` should run.
        If not specified the `evalPackages` default will be used (builtins.currentSystem or
        the `buildPackages` system).
        This argument is useful when using pure flake evalution (where builtins.currentSystem does
        not exist) when there are no builders present for the some or all of the flakes supported
        systems (so `buildPackages` also fails).
      '';
    };
    evalPackages = mkOption {
      type = attrs;
      default =
        if pkgs.system == config.evalSystem
          then pkgs
        else if pkgs.buildPackages.system == config.evalSystem
          then pkgs.buildPackages
        else
          import pkgs.path {
            system = config.evalSystem;
            overlays = pkgs.overlays;
          };
    };
    hsPkgs = lib.mkOption {
      type = lib.types.unspecified;
    };
  };
}
