{ pkgs }:
let
  span = pred: list:
    let n = pkgs.lib.lists.foldr (x: acc: if pred x then acc + 1 else 0) 0 list;
    in { fst = pkgs.lib.lists.take n list; snd = pkgs.lib.lists.drop n list; };

  # Look for a index-state: field in the cabal.project file
  parseIndexState = rawCabalProject:
      let
        indexState = pkgs.lib.lists.concatLists (
          pkgs.lib.lists.filter (l: l != null)
            (builtins.map (l: builtins.match "^index-state: *(.*)" l)
              (pkgs.lib.splitString "\n" rawCabalProject)));
      in
        pkgs.lib.lists.head (indexState ++ [ null ]);

  # Remove the indentation of the first line from all the lines
  unindent = blockLines: (pkgs.lib.foldl' ({matchString, out}: s:
    let
      m = builtins.match matchString s;
    in
      if m != null
        then {
          matchString = "(${builtins.head m})(.*)";
          out = out ++ [(builtins.elemAt m 1)];
        }
        else {
          inherit matchString out;
        }
    ) { matchString = "( *)(.*)"; out = []; } blockLines).out;

  # Strip comments except `--sha256`
  stripComments = blockLines: pkgs.lib.filter (s: s != "") (
    pkgs.lib.foldl' (out: s:
      let
        m = builtins.match "(.*)--.*" s;
      in
        if !(pkgs.lib.hasPrefix "--sha256" s) && m != null
          then out ++ [(builtins.head m)]
          else out ++ [s]
      ) [] blockLines
    );

  # Parse lines of a source-repository-package block
  parseBlockLines = blockLines: (pkgs.lib.foldl' ({name, attrs}: s:
    let
      # Look for a new attribute name
      pair = builtins.match "([^ :]*): *(.*)" s;

      # Function to build the next parse state when the attribute name is known
      nextState = name: value: {
          inherit name;
          # Support multi line attributes by appending the value to the named attribute
          attrs = attrs // {
            ${name} =
              if attrs ? ${name}
                then attrs.${name} + " " + value
                else value;
          };
        };
    in
      if pair != null
        then
          # First line of a new attribute
          nextState (builtins.head pair) (builtins.elemAt pair 1)
        else
          if name != null
            then nextState name s # Append another line to the current attribute
            else __trace "Expected attribute but found `${s}`" { inherit name attrs; }
    ) { name = null; attrs = {}; } (stripComments (unindent blockLines))).attrs;

  # Gets data for all the repositories to fetch
  # A comment like
  #   --shar256: 003lm3pm0000hbfmii7xcdd9v20000flxf7gdl2pyxia7p014i8z
  # will cause use of pkgs.fetchgit, else __fetchGit.
  extractRepoData = cabalProjectFileName: lookupSha256: repo:
    builtins.map (subdir:
      {
        url = repo.location;
        ref = repo.tag;
        sha256 = repo."--sha256" or (lookupSha256 repo);
        inherit subdir;
      })
      (if repo ? subdir
        then pkgs.lib.filter (x: x != "") (pkgs.lib.splitString " " repo.subdir)
        else ["."]);

  # Parse a source-repository-package and return data of `type: git` repositories
  parseBlock = cabalProjectFileName: lookupSha256: block:
    let
      x = span (pkgs.lib.strings.hasPrefix " ") (pkgs.lib.splitString "\n" block);
      attrs = parseBlockLines x.fst;
    in
      if attrs."type" or "" != "git"
        then {
          sourceRepo = [];
          otherText = "\nsource-repository-package\n" + block;
        }
        else {
          sourceRepo = extractRepoData cabalProjectFileName lookupSha256 attrs;
          otherText = pkgs.lib.strings.concatStringsSep "\n" x.snd;
        };

in {
  inherit parseIndexState parseBlockLines parseBlock;
}
