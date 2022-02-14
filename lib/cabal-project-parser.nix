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
  # will be trated like a field and returned here
  # (used in call-cabal-project-to-nix.nix to create a fixed-output derivation)
  extractSourceRepoPackageData = cabalProjectFileName: lookupSha256: repo: {
    url = repo.location;
    ref = repo.tag;
    sha256 = repo."--sha256" or (lookupSha256 repo);
    subdirs = if repo ? subdir
      then pkgs.lib.filter (x: x != "") (pkgs.lib.splitString " " repo.subdir)
      else ["."];
  };

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
          sourceRepo = extractSourceRepoPackageData cabalProjectFileName lookupSha256 attrs;
          otherText = pkgs.lib.strings.concatStringsSep "\n" x.snd;
        };

  parseSourceRepositoryPackages = cabalProjectFileName: lookupSha256: source-repo-override: projectFile:
    let
      blocks = pkgs.lib.splitString "\nsource-repository-package\n" ("\n" + projectFile);
      initialText = pkgs.lib.lists.take 1 blocks;
      repoBlocks = builtins.map (parseBlock cabalProjectFileName lookupSha256) (pkgs.lib.lists.drop 1 blocks);
      overrideSourceRepo = sourceRepo: (source-repo-override.${sourceRepo.url} or (pkgs.lib.id)) sourceRepo;
    in {
      sourceRepos = pkgs.lib.lists.map (block: overrideSourceRepo block.sourceRepo) repoBlocks;
      otherText = pkgs.lib.strings.concatStringsSep "\n" (
        initialText
        ++ (builtins.map (x: x.otherText) repoBlocks));
    };

  # Parse a repository
  parseRepositoryBlock = cabalProjectFileName: sha256map: cabal-install: nix-tools: block:
    let
      lines = pkgs.lib.splitString "\n" block;
      x = span (pkgs.lib.strings.hasPrefix " ") (__tail lines);
      attrs = parseBlockLines x.fst;
    in rec {
      name = __head lines;
      repo = attrs;
      home =
        pkgs.evalPackages.runCommandLocal name {
          nativeBuildInputs = [ cabal-install pkgs.evalPackages.curl nix-tools ];
          LOCALE_ARCHIVE = pkgs.lib.optionalString (pkgs.evalPackages.stdenv.buildPlatform.libc == "glibc") "${pkgs.evalPackages.glibcLocales}/lib/locale/locale-archive";
          LANG = "en_US.UTF-8";
          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
          outputHash = attrs."--sha256" or sha256map.${attrs.url};
        } ''
            mkdir -p $out/.cabal
            cat <<EOF > $out/.cabal/config
            repository ${name}
              url: ${attrs.url}
              ${pkgs.lib.optionalString (attrs ? secure) "secure: ${attrs.secure}"}
              ${pkgs.lib.optionalString (attrs ? root-keys) "root-keys: ${attrs.root-keys}"}
              ${pkgs.lib.optionalString (attrs ? key-threshold) "key-threshold: ${attrs.key-threshold}"}
            EOF

            export SSL_CERT_FILE=${pkgs.evalPackages.cacert}/etc/ssl/certs/ca-bundle.crt
            mkdir -p $out/.cabal/packages/${name}
            HOME=$out cabal new-update ${name}
            ls -ld $out/.cabal
        '';
      hackage = import (
        pkgs.evalPackages.runCommandLocal ("hackageg-to-nix-" + name) {
          nativeBuildInputs = [ cabal-install pkgs.evalPackages.curl nix-tools ];
          LOCALE_ARCHIVE = pkgs.lib.optionalString (pkgs.evalPackages.stdenv.buildPlatform.libc == "glibc") "${pkgs.evalPackages.glibcLocales}/lib/locale/locale-archive";
          LANG = "en_US.UTF-8";
        } ''
            mkdir -p $out
            hackage-to-nix $out ${home}/.cabal/packages/${name}/01-index.tar ${attrs.url}
        '');
      tarball = {
        inherit name;
        index = home + "/.cabal/packages/${name}/01-index.tar.gz";
      };
      otherText = ''
        repository ${name}
          url: file:${home + "/.cabal/packages/${name}"}
          secure: True
          root-keys:
          key-threshold: 0
        '' + pkgs.lib.strings.concatStringsSep "\n" x.snd;
    };

  parseRepositories = cabalProjectFileName: sha256map: cabal-install: nix-tools: projectFile:
    let
      blocks = pkgs.lib.splitString "\nrepository " ("\n" + projectFile);
      initialText = pkgs.lib.lists.take 1 blocks;
      repoBlocks = builtins.map (parseRepositoryBlock cabalProjectFileName sha256map cabal-install nix-tools) (pkgs.lib.lists.drop 1 blocks);
    in {
      extra-hackages = pkgs.lib.lists.map (block: block.hackage) repoBlocks;
      tarballs = pkgs.lib.lists.map (block: block.tarball) repoBlocks;
      otherText = pkgs.lib.strings.concatStringsSep "\n" (
        initialText
        ++ (builtins.map (x: x.otherText) repoBlocks));
    };

in {
  inherit parseIndexState parseSourceRepositoryPackages parseRepositories parseBlock;
}
