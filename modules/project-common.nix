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
    crossPlatforms = mkOption {
      type = unspecified;
      default = _p: [ ];
    };
    # Default shell arguments
    shell = mkOption {
      type = submodule [
        (import ./shell.nix { projectConfig = config; })
        { _module.args = { inherit pkgs; inherit (pkgs.haskell-nix) haskellLib; }; }
      ];
      default = { };
      description = ''
        Arguments to use for the default shell `p.shell` (these are passed to p.shellFor).
        For instance to include `cabal` and `ghcjs` support use
          shell = { tools.cabal = {}; crossPlatforms = p: [ p.ghcjs ]; }
      '';
    };
    # Default flake arguments
    flake = mkOption {
      type = submodule [
        (import ./flake.nix { projectConfig = config; })
        { _module.args = { inherit (pkgs.haskell-nix) haskellLib; }; }
      ];
      default = { };
      description = ''
        Default arguments to use for the `p.flake`.
      '';
    };
    evalSystem = mkOption {
      type = str;
      default = pkgs.pkgsBuildBuild.stdenv.system;
      description = ''
        Specifies the system on which `cabal` and `nix-tools` should run.
        If not specified the `pkgsBuildBuild` system will be used.
        If there are no builders for the `pkgsBuildBuild` system
        specifying a system for which there are builders will
        allow the evaluation of the haskell project to work.
      '';
    };
    evalPackages = mkOption {
      type = attrs;
      default =
        if pkgs.pkgsBuildBuild.stdenv.system == config.evalSystem
        then pkgs.pkgsBuildBuild
        else
          import pkgs.path {
            system = config.evalSystem;
            overlays = pkgs.overlays;
          };
      description = ''
        Packages used to run `cabal` and `nix-tools`.
        This will default to `pkgs.pkgsBuildBuild` if it
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
    evalSrc = mkOption {
      type = either path package;
      default = config.src;
      description = ''
        Allows a different version of the src to be used at eval time.
        This is useful when building the source may require a build machine.
        To avoid an eval time dependency on a build machine set `evalSrc`
        to either:
          * A version of the source built using `evalPackages`
          * A version of the source that does not require building
      '';
    };
    hsPkgs = lib.mkOption {
      type = lib.types.unspecified;
    };
    # Used by stack and cabal projects via
    # - ./lib/call-cabal-project-to-nix.nix
    # - ./lib/call-stack-to-nix.nix
    ignorePackageYaml = mkOption {
      type = bool;
      default = false;
      description = ''
        If set, prevents nix-tools from attempting to load package.yaml even if it is present.
      '';
    };
  };
}
