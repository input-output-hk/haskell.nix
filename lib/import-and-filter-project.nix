# This function takes the output of `callCabalProjectToNix` or
# `callStackToNix`.  It imports the projectNix that was produced
# and combines it with the original source.
{ pkgs, haskellLib }:
{ projectNix, sourceRepos, src }:
let
  # Full source including possible relative paths form the
  # project directory.
  srcRoot =
    if haskellLib.canCleanSource src
      then haskellLib.cleanSourceWith {
        name = if src ? name then "${src.name}-root" else "source-root";
        src = src.origSrc or src;
        filter = src.filter or (_: _: true);
      }
      else src.origSrc or src;
  # The sub directory containing the cabal.project or stack.yaml file
  projectSubDir' = src.origSubDir or "";                                     # With leading /
  projectSubDir = pkgs.lib.strings.removePrefix "/" projectSubDir';          # Without /
  projectSubDir'' = if projectSubDir == "" then "" else projectSubDir + "/"; # With trailing /
  project = import "${projectNix}${projectSubDir'}";
in project // {
    extras = hackage: let old = (project.extras hackage).packages; in {
      packages = pkgs.lib.attrsets.mapAttrs (name: value:
        if builtins.isFunction value
          then value
          else {...}@args: with pkgs.lib.strings;
            let
              oldPkg = import value args;
              # When the package src is in the project dir, this is the subDir it is in.
              subDir = removePrefix "/" (removePrefix (toString projectNix)
                                                      (toString oldPkg.src.content));
              srcRepoPrefix = projectSubDir'' + ".source-repository-packages/";

              packageInfo =
                if !hasPrefix (toString projectNix) (toString oldPkg.src.content)
                  then {
                    # Source location does not match project prefix
                    isProject = false;
                    packageSrc = toString oldPkg.src.content;
                  }
                else if hasPrefix srcRepoPrefix subDir
                  then {
                    # The source is from a source repository
                    isProject = false;
                    packageSrc = pkgs.lib.lists.elemAt sourceRepos (
                      toInt (removePrefix srcRepoPrefix subDir));
                  }
                else {
                    # Source does match project prefix and it is not from a source repository
                    isProject = true;
                    packageSrc = haskellLib.appendSubDir {
                      src = srcRoot;
                      inherit subDir;
                      includeSiblings = true; # Filtering sibling dirs of the package dir is done in the
                                              # component builder so that relative paths can be used to
                                              # reference project directories not in the package subDir.
                    };
                  };
            in oldPkg // {
              src = (pkgs.lib).mkDefault packageInfo.packageSrc;
              package = oldPkg.package // {
                isProject = (pkgs.lib).mkDefault packageInfo.isProject;
              };
            }) old;
    };
  }
