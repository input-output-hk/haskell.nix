# From https://github.com/NixOS/nix/issues/2944
{ lib, runCommand, git, cleanSourceWith }:
{ src, subDir ? "" }:

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
  filter_from_list = root: files:
    let
      all_paren_dirs = p:
        if p == "." || p == "/"
        then []
        else [ p ] ++ all_paren_dirs (dirOf p);

      whitelist_set = listToAttrs (
        concatMap (p:
          let full_path = toString (root + "/${p}"); in
          map (p': { name = p'; value = true; }) (all_paren_dirs full_path)
        ) files
      );
    in
    p: t: hasAttr (toString p) whitelist_set;

  has_prefix = prefix: s:
    prefix == builtins.substring 0 (builtins.stringLength prefix) s;
  remove_prefix = prefix: s:
    builtins.substring
      (builtins.stringLength prefix)
      (builtins.stringLength s - builtins.stringLength prefix)
      s;

  lines = s: filter (x : x != [] && x != "") (split "\n" s);
in

if builtins.pathExists (toString src + "/.git")
then
  let
    gitDir = cleanSourceWith ({
        filter = path: type:
          type == "directory" ||
          lib.any (i: (lib.hasSuffix i path)) [
            "/config" "/index" "/HEAD" ] ||
          (lib.strings.hasInfix "modules/" path &&
            lib.any (i: (lib.hasSuffix i path)) [
              "/config" "/index" "/HEAD" "/objects" "/refs" ]);
      } // (
      if builtins.pathExists (toString src + "/.git/index")
        then { inherit src; subDir = ".git"; }
        else {
          # likely a git worktree, so follow the indirection
          src =
            let
              git_content = lines (readFile (toString src + "/.git"));
              first_line = head git_content;
              prefix = "gitdir: ";
              ok = length git_content == 1 && has_prefix prefix first_line;
            in
              if ok
              then /. + remove_prefix prefix first_line
              else abort "gitSource.nix: Cannot parse ${toString src + "/.git"}";
    }));

    commonConfig = if builtins.pathExists (toString gitDir.origSrc + "/commondir")
        then 
           let
             git_content = lines (readFile (toString gitDir.origSrc + "/commondir"));
             first_line = head git_content;
           in toString gitDir.origSrc + "/" + first_line + "/config"
        else toString gitDir + "/config";

    gitModules = builtins.path { name = "gitmodules"; path = toString src + "/.gitmodules"; };

    whitelist_file =
      runCommand "git-ls-files" {envVariable = true;} ''
        tmp=$(mktemp -d)
        cd $tmp
        cp -r ${gitDir} .git
        chmod +w -R .git
        mkdir -p .git/objects .git/refs
        mkdir -p .git-common/objects .git-common/refs
        cp ${commonConfig} .git-common/config
        echo ../.git-common > .git/commondir
        cp ${gitModules} ./.gitmodules
        ${git}/bin/git ls-files --recurse-submodules | grep inline-js
        ${git}/bin/git ls-files --recurse-submodules > $out
      '';

    whitelist = lines (readFile (whitelist_file.out));

    filter = filter_from_list src whitelist;
  in
    cleanSourceWith {
      inherit src subDir filter;
    }

else
  trace "gitSource.nix: ${toString src} does not seem to be a git repository,\nassuming it is a clean checkout." (
    cleanSourceWith {
      inherit src subDir;
    }
  )
