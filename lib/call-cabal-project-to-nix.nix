{ dotCabal, pkgs, runCommand, nix-tools, cabal-install, ghc, hpack, symlinkJoin, cacert, index-state-hashes }:
let defaultGhc = ghc;
    defaultCabalInstall = cabal-install;
in { index-state ? null, index-sha256 ? null, src, ghc ? defaultGhc,
  cabal-install ? defaultCabalInstall, cabalProject ? null }:

# cabal-install versions before 2.4 will generate insufficient plan information.
assert (if (builtins.compareVersions cabal-install.version "2.4.0.0") < 0
         then throw "cabal-install (current version: ${cabal-install.version}) needs to be at least 2.4 for plan-to-nix to work without cabal-to-nix"
         else true);
let
  maybeCleanedSource =
    if pkgs.lib.canCleanSource src
    then pkgs.lib.cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        pkgs.lib.any (i: (pkgs.lib.hasSuffix i path)) [ ".project" ".cabal" "package.yaml" ]; }
    else src;

  # Using origSrc bypasses any cleanSourceWith so that it will work when
  # access to the store is restricted.  If origSrc was already in the store
  # you can pass the project in as a string.
  rawCabalProject =
    let origSrcDir = maybeCleanedSource.origSrc or maybeCleanedSource;
    in if cabalProject != null
    then cabalProject
    else
      if ((builtins.readDir origSrcDir)."cabal.project" or "") == "regular"
        then builtins.readFile (origSrcDir + "/cabal.project")
        else null;

  # Look for a index-state: field in the cabal.project file
  parseIndexState = rawCabalProject:
      let
        indexState = pkgs.lib.lists.concatLists (
          pkgs.lib.lists.filter (l: l != null)
            (builtins.map (l: builtins.match "^index-state: *(.*)" l)
              (pkgs.lib.splitString "\n" rawCabalProject)));
      in
        pkgs.lib.lists.head (indexState ++ [ null ]);

  index-state-found = if index-state != null
    then index-state
    else if rawCabalProject != null
      then parseIndexState rawCabalProject
      else null;

  # Lookup hash for the index state we found
  index-sha256-found = if index-sha256 != null
    then index-sha256
    else index-state-hashes.${index-state-found} or null;

in
  assert (if index-state-found == null
    then throw "No index state passed and none found in cabal.project" else true);
  assert (if index-sha256-found == null
    then throw "provided sha256 for index-state ${index-state-found} is null!" else true);

let
  span = pred: list:
    let n = pkgs.lib.lists.foldr (x: acc: if pred x then acc + 1 else 0) 0 list;
    in { fst = pkgs.lib.lists.take n list; snd = pkgs.lib.lists.drop n list; };

  # Parse lines of a source-repository-package block
  parseBlockLines = blockLines: builtins.listToAttrs (builtins.concatMap (s:
    let pair = builtins.match " *([^:]*): *(.*)" s;
    in pkgs.lib.optional (pair != null) (pkgs.lib.attrsets.nameValuePair
          (builtins.head pair)
          (builtins.elemAt pair 1))) blockLines);

  fetchRepo = repo: (pkgs.fetchgit {
    url = repo.location;
    rev = repo.tag;
    sha256 = repo."--sha256";
  }) + (if repo.subdir or "" == "" then "" else "/" + repo.subdir);

  # Parse a source-repository-package and fetch it if it containts
  # a line of the form
  #   --shar256: <<SHA256>>
  parseBlock = block:
    let
      x = span (pkgs.lib.strings.hasPrefix " ") (pkgs.lib.splitString "\n" block);
      attrs = parseBlockLines x.fst;  
    in
      if attrs."--sha256" or "" == ""
        then {
          sourceRepo = [];
          otherText = "\nsource-repository-package\n" + block;
        }
        else {
          sourceRepo = [ (fetchRepo attrs) ];
          otherText = pkgs.lib.strings.concatStringsSep "\n" x.snd;
        };

  # Deal with source-repository-packages in a way that will work in
  # restricted-eval mode (as long as a sha256 is included).
  # Replace source-repository-package blocks that have a sha256 with
  # packages: block containing nix sotre paths of the fetched repos.
  replaceSoureRepos = projectFile:
    let
      blocks = pkgs.lib.splitString "\nsource-repository-package\n" projectFile;
      initialText = pkgs.lib.lists.take 1 blocks;
      repoBlocks = builtins.map parseBlock (pkgs.lib.lists.drop 1 blocks);
      sourceRepos = pkgs.lib.lists.concatMap (x: x.sourceRepo) repoBlocks;
      otherText = pkgs.writeText "cabal.project" (pkgs.lib.strings.concatStringsSep "\n" (
        initialText
        ++ (builtins.map (x: x.otherText) repoBlocks)));
    in {
      inherit sourceRepos;
      makeFixedProjectFile = ''
        cp -f ${otherText} ./cabal.project
        chmod +w -R ./cabal.project
        echo "packages:" >> ./cabal.project
        mkdir -p ./.source-repository-packages
      '' +
        ( pkgs.lib.strings.concatStrings (
            pkgs.lib.lists.zipListsWith (n: f: ''
              mkdir -p ./.source-repository-packages/${builtins.toString n}
              rsync -a --prune-empty-dirs \
                --include '*/' --include '*.cabal' --include 'package.yaml' \
                --exclude '*' \
                 "${f}/" "./.source-repository-packages/${builtins.toString n}/"
              echo "  ./.source-repository-packages/${builtins.toString n}" >> ./cabal.project
            '')
              (pkgs.lib.lists.range 0 ((builtins.length fixedProject.sourceRepos) - 1))
              sourceRepos
          )
        );
    };

  fixedProject =
    if rawCabalProject == null
      then {
        sourceRepos = [];
        makeFixedProjectFile = "";
      }
      else replaceSoureRepos rawCabalProject;

  plan = runCommand "plan-to-nix-pkgs" {
    nativeBuildInputs = [ nix-tools ghc hpack cabal-install pkgs.rsync pkgs.git ];
  } (''
    tmp=$(mktemp -d)
    cd $tmp
    cp -r ${maybeCleanedSource}/* .
    chmod +w -R .
    ${fixedProject.makeFixedProjectFile}
    # warning: this may not generate the proper cabal file.
    # hpack allows globbing, and turns that into module lists
    # without the source available (we cleaneSourceWith'd it),
    # this may not produce the right result.
    find . -name package.yaml -exec hpack "{}" \;
    export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
    export GIT_SSL_CAINFO=${cacert}/etc/ssl/certs/ca-bundle.crt
    HOME=${dotCabal {
      index-state = index-state-found;
      sha256 = index-sha256-found; }} cabal new-configure \
        --with-ghc=${ghc.targetPrefix}ghc \
        --with-ghc-pkg=${ghc.targetPrefix}ghc-pkg \
        --enable-tests

    export LANG=C.utf8 # Needed or stack-to-nix will die on unicode inputs
    mkdir -p $out

    # ensure we have all our .cabal files (also those generated from package.yaml) files.
    # otherwise we'd need to be careful about putting the `cabal-generator = hpack` into
    # the nix expression.  As we already called `hpack` on all `package.yaml` files we can
    # skip that step and just package the .cabal files up as well.
    #
    # This is also important as `plan-to-nix` will look for the .cabal files when generating
    # the relevant `pkgs.nix` file with the local .cabal expressions.
    rsync -a --prune-empty-dirs \
          --include '*/' --include '*.cabal' --include 'package.yaml' \
          --exclude '*' \
          $tmp/ $out/

    # make sure the path's in the plan.json are relative to $out instead of $tmp
    # this is necessary so that plan-to-nix relative path logic can work.
    substituteInPlace $tmp/dist-newstyle/cache/plan.json --replace "$tmp" "$out"

    # run `plan-to-nix` in $out.  This should produce files right there with the
    # proper relative paths.
    (cd $out && plan-to-nix --plan-json $tmp/dist-newstyle/cache/plan.json -o .)

    # move pkgs.nix to default.nix ensure we can just nix `import` the result.
    mv $out/pkgs.nix $out/default.nix
  '');
in
  # TODO: We really want this (symlinks) instead of copying the source over each and
  #       every time.  However this will not work with sandboxed builds.  They won't
  #       have access to `plan` or `src` paths.  So while they will see all the
  #       links, they won't be able to read any of them.
  #
  #       We should be able to fix this if we propagaed the build inputs properly.
  #       As we are `import`ing the produced nix-path here, we seem to be losing the
  #       dependencies though.
  #
  #       I guess the end-result is that ifd's don't work well with symlinks.
  #
  # symlinkJoin {
  #   name = "plan-and-src";
  #   # todo: should we clean `src` to drop any .git, .nix, ... other irelevant files?
  #   buildInputs = [ plan src ];
  # }
  runCommand "plan-to-nix-pkgs-with-src" { nativeBuildInputs = [ pkgs.rsync ]; } (''
    mkdir $out
    # todo: should we clean `src` to drop any .git, .nix, ... other irelevant files?
    rsync -a "${src}/" "$out/"
    rsync -a ${plan}/ $out/
  '' +
    ( pkgs.lib.strings.concatStrings (
        pkgs.lib.lists.zipListsWith (n: f: ''
          mkdir -p $out/.source-repository-packages/${builtins.toString n}
          rsync -a "${f}/" "$out/.source-repository-packages/${builtins.toString n}/"
        '')
          (pkgs.lib.lists.range 0 ((builtins.length fixedProject.sourceRepos) - 1))
          fixedProject.sourceRepos
      )
    ) + ''
    # Rsync will have made $out read only and that can cause problems when
    # nix sandboxing is enabled (since it can prevent nix from moving the directory
    # out of the chroot sandbox).
    chmod +w $out
  '')
