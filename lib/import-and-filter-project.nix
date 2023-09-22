# This function takes the output of `callCabalProjectToNix` or
# `callStackToNix`.  It imports the projectNix that was produced
# and combines it with the original source.
{ pkgs, haskellLib }:
{ projectNix, sourceRepos, src }:
let
  # Full source including possible relative paths form the
  # project directory.
  srcRoot = haskellLib.cleanSourceWith {
    name = if src ? name then "${src.name}-root" else "source-root";
    src = src.origSrc or src;
    filter = src.filter or (_: _: true);
  };
  # The sub directory containing the cabal.project or stack.yaml file
  projectSubDir' = src.origSubDir or "";                                     # With leading /          # Without / # With trailing /
  project = import "${projectNix}${projectSubDir'}";
in project // {
    extras = hackage: let
      old = project.extras hackage;
    in old // {
      packages = pkgs.lib.attrsets.mapAttrs (_name: value:
        if builtins.isFunction value
          then value
          else {...}@args: with pkgs.lib.strings;
            let
              oldPkg = import value args;
              packageInfo =
                if oldPkg.src ? url
                  then {
                    # The source is from a source-repository-package in a cabal.project file
                    # and lib/call-cabal-project-to-nix.nix should have replaced the url with
                    # an index into the sourceRepos list.
                    isProject = false;
                    packageSrc = pkgs.lib.lists.elemAt sourceRepos (toInt oldPkg.src.url);
                  }
                else if !hasPrefix "${projectNix}" (toString oldPkg.src.content)
                  then {
                    # Source location does not match project prefix
                    isProject = false;
                    packageSrc = toString oldPkg.src.content;
                  }
                else {
                    # Source does match project prefix and it is not from a source repository
                    isProject = true;
                    packageSrc = haskellLib.appendSubDir {
                      src = srcRoot;
                      subDir = removePrefix "/" (removePrefix "${projectNix}"
                                                              (toString oldPkg.src.content));
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
            }) old.packages;
    };
  }
