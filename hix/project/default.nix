{ src
, userDefaults ? {}
, nixpkgs ? null
, nixpkgsPin ? null
, pkgs ? null
, checkMaterialization ? null
, compiler-nix-name ? null
, shell ? null
, ...}@commandArgs:
let
  inherit ((lib.evalModules {
    modules = [
      (import ../../modules/project-common.nix)
      (import ../../modules/stack-project.nix)
      (import ../../modules/cabal-project.nix)
      (import ../../modules/project.nix)
      (import ../../modules/hix-project.nix)
      projectDefaults
      commandArgs'
      { _module.args.pkgs = {}; }
    ];
  }).config) name;
  inherit (import ./../.. {}) sources;
  lib = import (sources.nixpkgs-unstable + "/lib");
  commandArgs' =
    builtins.listToAttrs (
      builtins.concatMap (
        name:
          if commandArgs.${name} == null || name == "src" || name == "userDefaults" || name == "inNixShell"
            then []
            else [{ inherit name; value = commandArgs.${name}; }]
    ) (builtins.attrNames commandArgs));
  importDefaults = src:
    if src == null || !(builtins.pathExists src)
      then {}
      else import src;
  userDefaults = importDefaults (commandArgs.userDefaults or null);
  projectDefaults = importDefaults (toString (src.origSrcSubDir or src) + "/nix/hix.nix");
  inherit ((lib.evalModules {
    modules = [
      (import ../../modules/project-common.nix)
      (import ../../modules/stack-project.nix)
      (import ../../modules/cabal-project.nix)
      (import ../../modules/project.nix)
      (import ../../modules/hix-project.nix)
      userDefaults
      projectDefaults
      commandArgs'
      ({config, pkgs, ...}: {
        haskellNix = import ./../.. { inherit checkMaterialization; };
        nixpkgsPin = "nixpkgs-unstable";
        nixpkgs = config.haskellNix.sources.${config.nixpkgsPin};
        nixpkgsArgs = config.haskellNix.nixpkgsArgs // {
          overlays = config.haskellNix.nixpkgsArgs.overlays ++ config.overlays;
        };
        _module.args.pkgs = import config.nixpkgs config.nixpkgsArgs;
        project = pkgs.haskell-nix.project [
            (import ../../modules/hix-project.nix)
            userDefaults
            projectDefaults
            commandArgs'
            ({config, ...}: {
              src =
                if builtins.pathExists (toString (src.origSrcSubDir or src) + "/.git")
                  then config.evalPackages.haskell-nix.haskellLib.cleanGit {
                    inherit src name;
                  }
                  else src;
            })
          ];
      })
    ];
  }).config) project;
in project
