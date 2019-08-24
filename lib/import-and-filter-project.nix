# This function takes the output of `callCabalProjectToNix` or
# `callStackToNix`.  It imports the projectNix that was produced
# and combines it with the original source.
{ pkgs, haskellLib }:
{ projectNix, sourceRepos, src }:
let
  project = import "${projectNix}";
in {
  nix = projectNix;
  pkgs = project // {
    extras = hackage: let old = (project.extras hackage).packages; in {
      packages = pkgs.lib.attrsets.mapAttrs (name: value:
        {...}@args:
          let oldPkg = import value args;
              packageSrc = if !pkgs.lib.strings.hasPrefix (toString projectNix) (toString oldPkg.src.content)
                then toString oldPkg.src.content
                else let
                  subDir = pkgs.lib.strings.removePrefix "/" (
                    pkgs.lib.strings.removePrefix (toString projectNix)
                      (toString oldPkg.src.content));
                  srcRepoPrefix = ".source-repository-packages/";
                  in if pkgs.lib.strings.hasPrefix srcRepoPrefix subDir
                    then
                      pkgs.lib.lists.elemAt sourceRepos (
                        pkgs.lib.strings.toInt (pkgs.lib.strings.removePrefix srcRepoPrefix subDir))
                    else if haskellLib.canCleanSource src
                      then haskellLib.cleanSourceWith { inherit src subDir; }
                      else src + (if subDir == "" then "" else "/" + subDir);
          in oldPkg // {
            src = (pkgs.lib).mkDefault packageSrc;
          }) old;
    };
  };
}
