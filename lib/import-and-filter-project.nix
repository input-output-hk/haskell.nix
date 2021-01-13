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
          else {...}@args:
            let oldPkg = import value args;
                packageSrc = if !pkgs.lib.strings.hasPrefix (toString projectNix) (toString oldPkg.src.content)
                  then toString oldPkg.src.content
                  else let
                    subDir = pkgs.lib.strings.removePrefix "/" (
                      pkgs.lib.strings.removePrefix (toString projectNix)
                        (toString oldPkg.src.content));
                    srcRepoPrefix = projectSubDir'' + ".source-repository-packages/";
                    in if pkgs.lib.strings.hasPrefix srcRepoPrefix subDir
                      then
                        pkgs.lib.lists.elemAt sourceRepos (
                          pkgs.lib.strings.toInt (pkgs.lib.strings.removePrefix srcRepoPrefix subDir))
                      else (haskellLib.appendSubDir {
                          src = srcRoot;
                          inherit subDir;
                          includeSiblings = true; # Filtering sibling dirs of the package dir is done in the
                                                  # component builder so that relative paths can be used to
                                                  # reference project directories not in the package subDir.
                        }) // {
                          # Tag the `src` attribute of the packages that are part of the project
                          # (but not source repository packages)
                          isProjectPackageSrc = true;
                        };
            in oldPkg // {
              src = (pkgs.lib).mkDefault packageSrc;
            }) old;
    };
  }
