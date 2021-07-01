{ src
, userDefaults ? {}
, nxipkgs ? null
, nixpkgsPin ? null
, pkgs ? null
, checkMaterialization ? null
, compiler-nix-name ? null
, shell ? null
, ...}@commandArgs:
let
  inherit ((lib.evalModules {
    modules = [
      (import ../modules/project-common.nix)
      (import ../modules/stack-project.nix)
      (import ../modules/cabal-project.nix)
      (import ../modules/project.nix)
      (import ../modules/hix-project.nix)
      projectDefaults
      commandArgs'
      { _module.args.pkgs = {}; }
    ];
  }).config) name;
  sources = import ../nix/sources.nix {};
  lib = import (sources.nixpkgs-unstable + "/lib");
  commandArgs' =
    builtins.listToAttrs (
      builtins.concatMap (
        name:
          if commandArgs.${name} == null || name == "src" || name == "userDefaults" || name == "inNixShell"
            then []
            else [{ inherit name; value = commandArgs.${name}; }]
    ) (builtins.attrNames commandArgs));
  defaultArgs = {
    nixpkgsPin = "nixpkgs-unstable";
  };
  importDefaults = src:
    if src == null || !(__pathExists src)
      then {}
      else import src;
  userDefaults = importDefaults (commandArgs.userDefaults or null);
  projectDefaults = importDefaults (toString (src.origSrcSubDir or src) + "/nix/hix.nix");
  inherit ((lib.evalModules {
    modules = [
      (import ../modules/project-common.nix)
      (import ../modules/stack-project.nix)
      (import ../modules/cabal-project.nix)
      (import ../modules/project.nix)
      (import ../modules/hix-project.nix)
      userDefaults
      projectDefaults
      commandArgs'
      ({config, pkgs, ...}: {
        haskellNix = import ./.. { inherit checkMaterialization; };
        nixpkgsPin = "nixpkgs-unstable";
        nixpkgs = config.haskellNix.sources.${config.nixpkgsPin};
        nixpkgsArgs = config.haskellNix.nixpkgsArgs // {
          overlays = config.haskellNix.nixpkgsArgs.overlays ++ config.overlays;
        };
        pkgs = import config.nixpkgs config.nixpkgsArgs;
        project = config.pkgs.haskell-nix.project [
            (import ../modules/hix-project.nix)
            userDefaults
            projectDefaults
            commandArgs'
            {
              src =
                if __pathExists (toString (src.origSrcSubDir or src) + "/.git")
                  then config.pkgs.haskell-nix.haskellLib.cleanGit {
                    inherit src name;
                  }
                  else src;
            }
          ];
      })
    ];
  }).config) project shell;
in project