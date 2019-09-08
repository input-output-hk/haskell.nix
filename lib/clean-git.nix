# From https://github.com/NixOS/nix/issues/2944
{ lib, runCommand, git, cleanSourceWith }:
{ src }:

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
    git_dir =
      if builtins.pathExists (toString src + "/.git/index")
      then (toString src + "/.git")
      else # likely a git worktree, so follow the indirection
        let
          git_content = lines (readFile (toString src + "/.git"));
          first_line = head git_content;
          prefix = "gitdir: ";
          ok = length git_content == 1 && has_prefix prefix first_line;
        in
          if ok
          then /. + remove_prefix prefix first_line
          else abort "gitSource.nix: Cannot parse ${toString src + "/.git"}";

    gitFiles = cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        lib.any (i: (lib.hasSuffix i path)) [
          ".gitmodules" ".git/config" ".git/index" ".git/HEAD" ".git/objects" ".git/refs" ] ||
        (lib.hasInfix ".git/modules/" path &&
          lib.any (i: (lib.hasSuffix i path)) [
            "config" "index" "HEAD" "objects" "refs" ]);
    };

    whitelist_file =
      runCommand "git-ls-files" {envVariable = true;} ''
        cd ${gitFiles}
        ${git}/bin/git ls-files --recurse-submodules > $out
      '';

    whitelist = lines (readFile (whitelist_file.out));

    filter = filter_from_list src whitelist;
  in
    cleanSourceWith {
      inherit src filter;
    }

else
  trace "gitSource.nix: ${toString src} does not seem to be a git repository,\nassuming it is a clean checkout." (
    cleanSourceWith {
      inherit src;
    }
  )