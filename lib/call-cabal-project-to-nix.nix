{ dotCabal, pkgs, runCommand, nix-tools, cabal-install, ghc, hpack, symlinkJoin, cacert, index-state-hashes, haskellLib, materialize }@defaults:
{ name          ? src.name or (baseNameOf src) # optional name for better error messages
, src
, index-state   ? null # Hackage index-state, eg. "2019-10-10T00:00:00Z"
, index-sha256  ? null # The hash of the truncated hackage index-state
, plan-sha256   ? null # The hash of the plan-to-nix output (makes the plan-to-nix step a fixed output derivation)
, materialized  ? null # Location of a materialized copy of the nix files
, checkMaterialization ? null # If true the nix files will be generated used to check plan-sha256 and material
, cabalProject  ? null # Cabal project file (when null uses "${src}/cabal.project")
, ghc           ? defaults.ghc
, nix-tools     ? defaults.nix-tools
, hpack         ? defaults.hpack
, cabal-install ? defaults.cabal-install
, configureArgs ? "" # Extra arguments to pass to `cabal new-configure` (--enable-tests is included by default, include `--disable-tests` to override that)
, ...
}@args:
# cabal-install versions before 2.4 will generate insufficient plan information.
assert (if (builtins.compareVersions cabal-install.version "2.4.0.0") < 0
         then throw "cabal-install (current version: ${cabal-install.version}) needs to be at least 2.4 for plan-to-nix to work without cabal-to-nix"
         else true);

assert (if ghc.isHaskellNixCompiler or false then true
  else throw ("It is likely you used `haskell.compiler.X` instead of `haskell-nix.compiler.X`"
    + pkgs.lib.optionalString (name != null) (" for " + name)));

let
  maybeCleanedSource =
    if haskellLib.canCleanSource src
    then haskellLib.cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        pkgs.lib.any (i: (pkgs.lib.hasSuffix i path)) [ ".project" ".cabal" "package.yaml" ]; }
    else src;

  # Using origSrcSubDir bypasses any cleanSourceWith so that it will work when
  # access to the store is restricted.  If origSrc was already in the store
  # you can pass the project in as a string.
  rawCabalProject =
    let origSrcDir = maybeCleanedSource.origSrcSubDir or maybeCleanedSource;
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
    else
      let cabalProjectIndexState = if rawCabalProject != null
        then parseIndexState rawCabalProject
        else null;
      in
        if cabalProjectIndexState != null
          then cabalProjectIndexState
          else builtins.trace ("Using latest index state" + (if name == null then "" else " for " + name) + "!")
            (pkgs.lib.last (builtins.attrNames index-state-hashes));

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

  plan-nix = materialize ({
    inherit materialized;
    sha256 = plan-sha256;
    sha256Arg = "plan-sha256";
    # Before pinning stuff down we need an index state to use
    reasonNotSafe =
      if index-state == null
        then "index-state is not set"
        else null;
  } // pkgs.lib.optionalAttrs (checkMaterialization != null) {
    inherit checkMaterialization;
  }) (builtins.trace "[${if name == null then "plan-to-nix-pkgs" else name + "-plan-to-nix-pkgs"}] cabal new-configure --with-ghc=${ghc.targetPrefix}ghc --with-ghc-pkg=${ghc.targetPrefix}ghc-pkg" (runCommand (if name == null then "plan-to-nix-pkgs" else name + "-plan-to-nix-pkgs") {
    nativeBuildInputs = [ nix-tools ghc hpack cabal-install pkgs.rsync pkgs.git ];
    # Needed or stack-to-nix will die on unicode inputs
    LANG = "en_US.UTF-8";
    meta.platforms = pkgs.lib.platforms.all;
    preferLocalBuild = false;
  } ''
    tmp=$(mktemp -d)
    cd $tmp
    # if maybeCleanedSource is empty, this means it's a new
    # project where the files haven't been added to the git
    # repo yet. We fail early and provide a useful error
    # message to prevent headaches (#290).
    if [ -z "$(ls -A ${maybeCleanedSource})" ]; then
      echo "cleaned source is empty. Did you forget to 'git add -A'?"; exit 1;
    fi
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
      inherit cabal-install nix-tools;
      index-state =
        builtins.trace ("Using index-state: ${index-state-found}" + (if name == null then "" else " for " + name))
          index-state-found;
      sha256 = index-sha256-found; }} cabal new-configure \
        --with-ghc=${ghc.targetPrefix}ghc \
        --with-ghc-pkg=${ghc.targetPrefix}ghc-pkg \
        --enable-tests \
        ${configureArgs}

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
    (cd $out && plan-to-nix --full --plan-json $tmp/dist-newstyle/cache/plan.json -o .)

    # Remove the non nix files ".project" ".cabal" "package.yaml" files
    # as they should not be in the output hash (they may change slightly
    # without affecting the nix).
    if [ -d $out/.source-repository-packages ]; then
      chmod +w -R $out/.source-repository-packages
      rm -rf $out/.source-repository-packages
    fi
    find $out -type f ! -name '*.nix' -delete
    # Remove empty dirs
    find $out -type d -empty -delete

    # move pkgs.nix to default.nix ensure we can just nix `import` the result.
    mv $out/pkgs.nix $out/default.nix
  ''));
in { projectNix = plan-nix; inherit src; inherit (fixedProject) sourceRepos; }
