# From https://github.com/NixOS/nix/issues/2944
{ lib, runCommand, git, cleanSourceWith }:
{ name ? null, src, subDir ? "", includeSiblings ? false, keepGitDir ? false }:

# The function call
#
#   gitSource ./toplevel subpath
#
# creates a Nix store path of ./toplevel/subpath that includes only those files
# tracked by git. More precisely: mentioned in the git index (i.e. git add is enough
# to get them to be included, you do not have to commit).
#
# This is a whitelist-based alternative to manually listing files or using
# nix-gitignore.

# Internally, it works by calling git ls-files at evaluation time. To
# avoid copying all of `.git` to the git store, it only copies the least amount
# of files necessary for `git ls-files` to work; this is a bit fragile, but
# very fast.

with builtins;

# We read the git index once, before getting the subdir parameter, so that it
# is shared among multiple invocations of gitSource:

let
  remove_prefix = prefix: s:
    builtins.substring
      (builtins.stringLength prefix)
      (builtins.stringLength s - builtins.stringLength prefix)
      s;

  lines = s: filter (x : x != [] && x != "") (split "\n" s);

  origSrcSubDir = toString (src.origSrcSubDir or src);
in

if builtins.pathExists (origSrcSubDir + "/.git")
then
  let
    hasIndex = builtins.pathExists (origSrcSubDir + "/.git/index");
    isWorktree = (builtins.readDir origSrcSubDir).".git" == "regular";

    # Identify the .git directory and filter just the files that we need.
    gitDir = cleanSourceWith ({
        caller = "cleanGit";
        name = (if name == null then "" else name + "-") + "gitFiles";
        filter = path: type:
          type == "directory" ||
          lib.any (i: (lib.hasSuffix i path)) [
            "/config" "/index" "/HEAD" ] ||
          (lib.strings.hasInfix "modules/" path &&
            lib.any (i: (lib.hasSuffix i path)) [
              "/config" "/index" "/HEAD" "/objects" "/refs" ]);
      } // (
      if hasIndex
        then { inherit src; subDir = ".git"; }
        else if !isWorktree
          then abort "cleanGit: ${origSrcSubDir + "/.git"} has no index file"
          else {
            # likely a git worktree, so follow the indirection
            src =
              let
                git_content = lines (readFile (origSrcSubDir + "/.git"));
                first_line = head git_content;
                prefix = "gitdir: ";
                ok = length git_content == 1 && lib.hasPrefix prefix first_line;
              in
                if ok
                then /. + remove_prefix prefix first_line
                else abort "gitSource.nix: Cannot parse ${origSrcSubDir + "/.git"}";
    }));

    # Worktrees have a commondir pointing to the common `.git` dir.  We need the
    # config file from there to get the list of active submodules right.
    commonConfig = if builtins.pathExists (toString gitDir.origSrc + "/commondir")
        then
           # The commondir file contains just the relative path of the
           # common `.git` dir.
           let
             git_content = lines (readFile (toString gitDir.origSrc + "/commondir"));
             first_line = head git_content;
           in gitDir.origSrc + ("/" + first_line + "/config")
        else gitDir + "/config";

    # We need the .gitmodules file for submoules to work.
    gitModulesStr = origSrcSubDir + "/.gitmodules";
    gitModules = builtins.path { name = "gitmodules"; path = gitModulesStr; };

    gitSubmoduleFiles = cleanSourceWith {
      caller = "cleanGit";
      name = (if name == null then "" else name + "-") + "gitSubmoduleFiles";
      inherit src;
      filter = path: type:
          type == "directory" # TODO get sudmodule directories from `.gitmodules`
                              # and use that to filter directory tree here
        ||
          lib.any (i: (lib.hasSuffix i path)) [
            "/.git" ".gitmodules" ".git/config" ".git/index" ".git/HEAD" ".git/objects" ".git/refs" ] ||
          (lib.strings.hasInfix ".git/modules/" path &&
            lib.any (i: (lib.hasSuffix i path)) [
              "config" "index" "HEAD" "objects" "refs" ]);
    };

    # Make a temporary dir that looks enough like the real thing for
    # `git ls-files --recurse-submodules` to give us an accurate list
    # of all the files in the index.
    whitelist_file =
      runCommand "git-ls-files" {envVariable = true;} ''
        tmp=$(mktemp -d)
        cd $tmp
        ${ lib.optionalString (!isWorktree && builtins.pathExists gitModulesStr) ''
          cp -ra ${gitSubmoduleFiles}/. $tmp
          chmod +w -R $tmp
          rm -rf .git
        ''}
        cp -r ${gitDir} .git
        chmod +w -R .git
        mkdir -p .git/objects .git/refs
        mkdir -p .git-common/objects .git-common/refs
        cp ${commonConfig} .git-common/config
        echo ../.git-common > .git/commondir
        ${ lib.optionalString (isWorktree && builtins.pathExists gitModulesStr) ''
          cp ${gitModules} ./.gitmodules
        ''}
        ${git}/bin/git ls-files --recurse-submodules > $out
      '';

    whitelist = lines (readFile (whitelist_file.out));

    all_paren_dirs = p:
        if p == "." || p == "/"
        then []
        else [ p ] ++ all_paren_dirs (dirOf p);

    # All the paths that we need to keep as a set (including parent dirs)
    whitelist_set = listToAttrs (
        concatMap (p:
          # Using `origSrcSubDir` (if present) makes it possible to cleanGit src that
          # has already been cleaned with cleanSrcWith.
          let full_path = src.origSrcSubDir or (toString src) + "/${p}"; in
          map (p': { name = p'; value = true; }) (all_paren_dirs full_path)
        ) whitelist
      );

    # Identify files in the `.git` dir
    isGitDirPath = path: 
          path == origSrcSubDir + "/.git"
        || lib.hasPrefix (origSrcSubDir + "/.git/") path;

    filter = path: type:
         hasAttr (toString path) whitelist_set
      || (keepGitDir && isGitDirPath path);
  in
    cleanSourceWith {
      caller = "cleanGit";
      inherit name src subDir includeSiblings filter;
    }

else
  trace "haskell-nix.haskellLib.cleanGit: ${origSrcSubDir} does not seem to be a git repository,\nassuming it is a clean checkout." (
    cleanSourceWith {
      caller = "cleanGit";
      inherit name src subDir includeSiblings;
    }
  )
