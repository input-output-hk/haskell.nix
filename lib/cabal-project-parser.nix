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
  extractSourceRepoPackageData = cabalProjectFileName: sha256map: repo:
    let
      refOrRev =
        if builtins.match "[0-9a-f]{40}" repo.tag != null
          then "rev"
          else "ref";
    in {
    url = repo.location;
    "${refOrRev}" = repo.tag;
    sha256 = repo."--sha256" or (
      if sha256map != null && sha256map ? ${repo.location}
        then sha256map.${repo.location}.${repo.tag}
        else null);
    subdirs = if repo ? subdir
      then pkgs.lib.filter (x: x != "") (pkgs.lib.splitString " " repo.subdir)
      else ["."];
  };

  # Parse a source-repository-package and return data of `type: git` repositories
  # See tests/unit.nix for examples of input and output.
  parseSourceRepositoryPackageBlock = cabalProjectFileName: sha256map: block:
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
          sourceRepo = extractSourceRepoPackageData cabalProjectFileName sha256map attrs;
          otherText = pkgs.lib.strings.concatStringsSep "\n" x.snd;
        };

  parseSourceRepositoryPackages = cabalProjectFileName: sha256map: source-repo-override: projectFile:
    let
      blocks = pkgs.lib.splitString "\nsource-repository-package\n" ("\n" + projectFile);
      initialText = pkgs.lib.lists.take 1 blocks;
      repoBlocks = builtins.map (parseSourceRepositoryPackageBlock cabalProjectFileName sha256map) (pkgs.lib.lists.drop 1 blocks);
      overrideSourceRepo = sourceRepo: (source-repo-override.${sourceRepo.url} or (pkgs.lib.id)) sourceRepo;
    in {
      sourceRepos = pkgs.lib.lists.map (block: overrideSourceRepo block.sourceRepo) repoBlocks;
      otherText = pkgs.lib.strings.concatStringsSep "\n" (
        initialText
        ++ (builtins.map (x: x.otherText) repoBlocks));
    };

  # Parse and replace repository
  # This works in a similar way to the `source-repository-package` but we are
  # able to simply replace the `repository` blocks with local `file:/nix/store` ones.
  # This works because `cabal configure` does not include any of the `/nix/sore/`
  # paths in the `plan.json` (so materialized plan-nix will still work as expeced).
  # See tests/unit.nix for examples of input and output.
  parseRepositoryBlock = evalPackages: cabalProjectFileName: sha256map: inputMap: cabal-install: nix-tools: block:
    let
      lines = pkgs.lib.splitString "\n" block;
      # The first line will contain the repository name.
      x = span (pkgs.lib.strings.hasPrefix " ") (__tail lines);
      attrs = parseBlockLines x.fst;
      sha256 = attrs."--sha256" or (
        if sha256map != null
          then sha256map.${attrs.url} or null
          else null);
    in rec {
      # This is `some-name` from the `repository some-name` line in the `cabal.project` file.
      name = __head lines;
      # The $HOME/.cabal/packages/${name} after running `cabal v2-update` to download the repository
      repoContents = if inputMap ? ${attrs.url}
        # If there is an input use it to make `file:` url and create a suitable `.cabal/packages/${name}` directory
        then evalPackages.runCommand name ({
          nativeBuildInputs = [ cabal-install ] ++ evalPackages.haskell-nix.cabal-issue-8352-workaround;
          preferLocalBuild = true;
        }) ''
            HOME=$(mktemp -d)
            mkdir -p $HOME/.cabal/packages/${name}
            cat <<EOF > $HOME/.cabal/config
            repository ${name}
              url: file:${inputMap.${attrs.url}}
              ${pkgs.lib.optionalString (attrs ? secure) "secure: ${attrs.secure}"}
              ${pkgs.lib.optionalString (attrs ? root-keys) "root-keys: ${attrs.root-keys}"}
              ${pkgs.lib.optionalString (attrs ? key-threshold) "key-threshold: ${attrs.key-threshold}"}
            EOF
            cabal v2-update ${name}
            cp -r $HOME/.cabal/packages/${name} $out
        ''
        else evalPackages.runCommand name ({
          nativeBuildInputs = [ cabal-install evalPackages.curl nix-tools ] ++ evalPackages.haskell-nix.cabal-issue-8352-workaround;
          LOCALE_ARCHIVE = pkgs.lib.optionalString (evalPackages.stdenv.buildPlatform.libc == "glibc") "${evalPackages.glibcLocales}/lib/locale/locale-archive";
          LANG = "en_US.UTF-8";
          preferLocalBuild = true;
        } // pkgs.lib.optionalAttrs (sha256 != null) {
          outputHashMode = "recursive";
          outputHashAlgo = "sha256";
          outputHash = sha256;
        }) ''
            HOME=$(mktemp -d)
            mkdir -p $HOME/.cabal/packages/${name}
            cat <<EOF > $HOME/.cabal/config
            repository ${name}
              url: ${attrs.url}
              ${pkgs.lib.optionalString (attrs ? secure) "secure: ${attrs.secure}"}
              ${pkgs.lib.optionalString (attrs ? root-keys) "root-keys: ${attrs.root-keys}"}
              ${pkgs.lib.optionalString (attrs ? key-threshold) "key-threshold: ${attrs.key-threshold}"}
            EOF
            export SSL_CERT_FILE=${evalPackages.cacert}/etc/ssl/certs/ca-bundle.crt
            cabal v2-update ${name}
            cp -r $HOME/.cabal/packages/${name} $out
        '';
      # Output of hackage-to-nix
      hackage = import (
        evalPackages.runCommand ("hackage-to-nix-" + name) {
          nativeBuildInputs = [ cabal-install evalPackages.curl nix-tools ];
          LOCALE_ARCHIVE = pkgs.lib.optionalString (evalPackages.stdenv.buildPlatform.libc == "glibc") "${evalPackages.glibcLocales}/lib/locale/locale-archive";
          LANG = "en_US.UTF-8";
          preferLocalBuild = true;
        } ''
          mkdir -p $out
          hackage-to-nix $out ${repoContents}/01-index.tar ${attrs.url}
        '');
      # Directory to `lndir` when constructing a suitable $HOME/.cabal dir
      repo = {
        # Strings used as attrset keys can't have contet. This can cause problems if the cabal.project file has antiquoted strings
        # in it. Discarding the context here works, and because 'name' is used elsewhere, we don't actually lose the string content,
        # which can matter!
        ${builtins.unsafeDiscardStringContext name} = repoContents;
      };
    };

  parseRepositories = evalPackages: cabalProjectFileName: sha256map: inputMap: cabal-install: nix-tools: projectFile:
    let
      # This will leave the name of repository in the first line of each block
      blocks = pkgs.lib.splitString "\nrepository " ("\n" + projectFile);
      repoBlocks = builtins.map (parseRepositoryBlock evalPackages cabalProjectFileName sha256map inputMap cabal-install nix-tools) (pkgs.lib.lists.drop 1 blocks);
    in {
      extra-hackages = pkgs.lib.lists.map (block: block.hackage) repoBlocks;
      repos = pkgs.lib.lists.foldl' (x: block: x // block.repo) {} repoBlocks;
    };

in {
  inherit parseIndexState parseSourceRepositoryPackages parseRepositories
    # These are only exposed for tests
    parseSourceRepositoryPackageBlock parseRepositoryBlock;
}
