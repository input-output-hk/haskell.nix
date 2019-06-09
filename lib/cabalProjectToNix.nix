{ dotCabal, pkgs, runCommand, nix-tools, cabal-install, ghc, hpack, symlinkJoin, cacert, index-state-hashes }:
let defaultGhc = ghc;
    defaultCabalInstall = cabal-install;
in { index-state, index-sha256 ? index-state-hashes.${index-state} or null, src, ghc ? defaultGhc,
  cabal-install ? defaultCabalInstall, cabalProject ? null }:

# better error message than just assert failed.
assert (if index-sha256 == null then throw "provided sha256 for index-state ${index-state} is null!" else true);
# cabal-install versions before 2.4 will generate insufficient plan information.
assert (if (builtins.compareVersions cabal-install.version "2.4.0.0") < 0
         then throw "cabal-install (current version: ${cabal-install.version}) needs to be at least 2.4 for plan-to-nix to work without cabal-to-nix"
         else true);
let
  cabalFiles =
    pkgs.lib.cleanSourceWith {
      inherit src;
      filter = path: type:
        type == "directory" ||
        pkgs.lib.any (i: (pkgs.lib.hasSuffix i path)) [ ".project" ".cabal" "package.yaml" ];
    };

  span = pred: list:
    let n = pkgs.lib.lists.foldr (x: acc: if pred x then acc + 1 else 0) 0 list;
    in { fst = pkgs.lib.lists.take n list; snd = pkgs.lib.lists.drop n list; };

  # Parse lines of a source-repository-package block
  parseBlockLines = blockLines: builtins.listToAttrs (builtins.map (s:
    let pair = builtins.match " *([^:]*): *(.*)" s;
    in pkgs.lib.attrsets.nameValuePair
          (builtins.head pair)
          (builtins.elemAt pair 1)) blockLines);

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
          otherText = block;
        }
        else {
          sourceRepo = [ (fetchRepo attrs) ];
          otherText = pkgs.lib.strings.concatStringsSep "\n" x.snd;
        };

  # Replace source-repository-package blocks that have a sha256 with
  # packages: block containing nix sotre paths of the fetched repos.
  replaceSoureRepos = projectFile:
    let
      blocks = pkgs.lib.splitString "\nsource-repository-package\n" projectFile;
      initialText = pkgs.lib.lists.take 1 blocks;
      repoBlocks = builtins.map parseBlock (pkgs.lib.lists.drop 1 blocks);
      sourceRepos = pkgs.lib.lists.concatMap (x: x.sourceRepo) repoBlocks;
    in {
      otherText = pkgs.lib.strings.concatStringsSep "\n" (
        initialText
        ++ (builtins.map (x: x.otherText) repoBlocks));
      inherit sourceRepos;
    };

  fixedProject = replaceSoureRepos
    (if cabalProject != null
      then cabalProject
      else builtins.readFile (cabalFiles + "/cabal.project"));

  # Deal with source-repository-packages in a way that will work on
  # hydra build agents (as long as a sha256 is included).
  fixedProjectFile = pkgs.writeText "cabal.project" fixedProject.otherText;
  
  plan = runCommand "plan" {
    nativeBuildInputs = [ nix-tools ghc hpack cabal-install pkgs.rsync pkgs.git ];
  } ''
    tmp=$(mktemp -d)
    cd $tmp
    cp -r ${cabalFiles}/* .
    chmod +w -R .
    cp -f ${fixedProjectFile} ./cabal.project
    chmod +w -R ./cabal.project
    echo "packages:" >> ./cabal.project
    mkdir -p ./.source-repository-packages
  '' +
    ( pkgs.lib.strings.concatStrings (
        pkgs.lib.lists.zipListsWith (n: f: ''
          cp -r ${f} ./.source-repository-packages/${builtins.toString n}
          echo "  ./.source-repository-packages/${builtins.toString n}" >> ./cabal.project
        '')
          (pkgs.lib.lists.range 0 ((builtins.length fixedProject.sourceRepos) - 1))
          fixedProject.sourceRepos
      )
    ) + ''
    # warning: this may not generate the proper cabal file.
    # hpack allows globbing, and turns that into module lists
    # without the source available (we cleaneSourceWith'd it),
    # this may not produce the right result.
    find . -name package.yaml -exec hpack "{}" \;
    export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
    export GIT_SSL_CAINFO=${cacert}/etc/ssl/certs/ca-bundle.crt
    HOME=${dotCabal { inherit index-state; sha256 = index-sha256; }} cabal new-configure

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
  '';
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
  runCommand "plan-and-src" { nativeBuildInputs = [ pkgs.rsync ]; } ''
    mkdir $out
    # todo: should we clean `src` to drop any .git, .nix, ... other irelevant files?
    rsync -a "${src}/" "$out/"
    rsync -a ${plan}/ $out/
    # Rsync will have made $out read only and that can cause problems when
    # nix sandboxing is enabled (since it can prevent nix from moving the directory
    # out of the chroot sandbox).
    chmod +w $out
  ''
