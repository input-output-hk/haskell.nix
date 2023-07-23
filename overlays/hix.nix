final: prev: { haskell-nix = prev.haskell-nix // { hix = {
  project =
    { src
    , userDefaults ? {}
    , subDir ? null
    , name ? null
    , compiler-nix-name ? null
    , shell ? null
    , ...}@commandArgs:
    let
      inherit (final) lib;
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
      commandArgs' =
        builtins.listToAttrs (
          builtins.concatMap (
            name:
              if commandArgs.${name} == null || name == "src" || name == "userDefaults"
                then []
                else [{ inherit name; value = commandArgs.${name}; }]
        ) (builtins.attrNames commandArgs));
      importDefaults = src:
        if src == null || !(builtins.pathExists src)
          then {}
          else import src;
      projectDefaults = importDefaults (toString (src.origSrcSubDir or src) + "/nix/hix.nix");
    in final.haskell-nix.project' [
            (import ../modules/hix-project.nix)
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
}; }; }
