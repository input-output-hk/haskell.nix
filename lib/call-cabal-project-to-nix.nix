{ dotCabal, pkgs, runCommand, evalPackages, symlinkJoin, cacert, index-state-hashes, haskellLib, materialize }@defaults:
let readIfExists = src: fileName:
      let origSrcDir = src.origSrcSubDir or src;
      in
        if builtins.elem ((__readDir origSrcDir)."${fileName}" or "") ["regular" "symlink"]
          then __readFile (origSrcDir + "/${fileName}")
          else null;
in
{ name          ? src.name or null # optional name for better error messages
, src
, compiler-nix-name    # The name of the ghc compiler to use eg. "ghc884"
, index-state   ? null # Hackage index-state, eg. "2019-10-10T00:00:00Z"
, index-sha256  ? null # The hash of the truncated hackage index-state
, plan-sha256   ? null # The hash of the plan-to-nix output (makes the plan-to-nix step a fixed output derivation)
, materialized  ? null # Location of a materialized copy of the nix files
, checkMaterialization ? null # If true the nix files will be generated used to check plan-sha256 and material
, cabalProjectFileName ? "cabal.project"
, cabalProject         ? readIfExists src cabalProjectFileName
, cabalProjectLocal    ? readIfExists src "${cabalProjectFileName}.local"
, cabalProjectFreeze   ? readIfExists src "${cabalProjectFileName}.freeze"
, caller               ? "callCabalProjectToNix" # Name of the calling funcion for better warning messages
, ghc           ? null # Deprecated in favour of `compiler-nix-name`
, ghcOverride   ? null # Used when we need to set ghc explicitly during bootstrapping
, nix-tools     ? evalPackages.haskell-nix.nix-tools.${compiler-nix-name}     # When building cabal projects we use the nix-tools
, cabal-install ? evalPackages.haskell-nix.cabal-install.${compiler-nix-name} # and cabal-install compiled with matching ghc version
, configureArgs ? "" # Extra arguments to pass to `cabal v2-configure`.
                     # `--enable-tests --enable-benchmarks` are included by default.
                     # If the tests and benchmarks are not needed and they
                     # causes the wrong plan to be choosen, then we can use
                     # `configureArgs = "--disable-tests --disable-benchmarks";`
, sha256map     ? null
                     # An alternative to adding `--sha256` comments into the
                     # cabal.project file:
                     #   sha256map =
                     #     { "https://github.com/jgm/pandoc-citeproc"."0.17"
                     #         = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; };
, lookupSha256  ?
  if sha256map != null
    then { location, tag, ...}: sha256map."${location}"."${tag}"
    else _: null
, extra-hackage-tarballs ? []
, ...
}@args:

let
  forName = pkgs.lib.optionalString (name != null) (" for " + name);

  ghc' =
    if ghcOverride != null
      then ghcOverride
      else
        if ghc != null
          then __trace ("WARNING: A `ghc` argument was passed" + forName
            + " this has been deprecated in favour of `compiler-nix-name`. "
            + "Using `ghc` will break cross compilation setups, as haskell.nix cannot "
            + "pick the correct `ghc` package from the respective buildPackages. "
            + "For example, use `compiler-nix-name = \"ghc865\";` for GHC 8.6.5.") ghc
          else
              # Do note that `pkgs = final.buildPackages` in the `overlays/haskell.nix`
              # call to this file. And thus `pkgs` here is the proper `buildPackages`
              # set and we do not need, nor should pick the compiler from another level
              # of `buildPackages`, lest we want to get confusing errors about the Win32
              # package.
              #
              # > The option `packages.Win32.package.identifier.name' is used but not defined.
              #
              pkgs.haskell-nix.compiler."${compiler-nix-name}";

in
  assert (if ghc'.isHaskellNixCompiler or false then true
    else throw ("It is likely you used `haskell.compiler.X` instead of `haskell-nix.compiler.X`"
      + forName));

let
  ghc = ghc';
  subDir' = src.origSubDir or "";
  subDir = pkgs.lib.strings.removePrefix "/" subDir';
  maybeCleanedSource =
    if haskellLib.canCleanSource src
      then (haskellLib.cleanSourceWith {
        name = if name != null then "${name}-root-cabal-files" else "source-root-cabal-files";
        src = src.origSrc or src;
        filter = path: type: (!(src ? filter) || src.filter path type) && (
          type == "directory" ||
          pkgs.lib.any (i: (pkgs.lib.hasSuffix i path)) [ ".cabal" "package.yaml" ]); })
      else src.origSrc or src;

  # Using origSrcSubDir bypasses any cleanSourceWith so that it will work when
  # access to the store is restricted.  If origSrc was already in the store
  # you can pass the project in as a string.
  rawCabalProject =
    if cabalProject != null
      then cabalProject + (
        if cabalProjectLocal != null
          then ''

            -- Added from cabalProjectLocal argument to cabalProject
            ${cabalProjectLocal}
          ''
          else ""
      )
      else null;

  index-state-found = if index-state != null
    then index-state
    else
      let cabalProjectIndexState = if rawCabalProject != null
        then pkgs.haskell-nix.haskellLib.parseIndexState rawCabalProject
        else null;
      in
        if cabalProjectIndexState != null
          then cabalProjectIndexState
          else builtins.trace ("Using latest index state" + (if name == null then "" else " for " + name) + "!")
            (pkgs.lib.last (builtins.attrNames index-state-hashes));

in
  assert (if index-state-found == null
    then throw "No index state passed and none found in ${cabalProjectFileName}" else true);

  assert (if index-sha256 == null && !(pkgs.lib.hasSuffix "Z" index-state-found)
    then throw "Index state found was ${index-state-found} and no `index-sha256` was provided. "
      "The index hash lookup code requires zulu time zone (ends in a Z)" else true);

let
  # If a hash was not specified find a suitable cached index state to
  # use that will contain all the packages we need.  By using the
  # first one after the desired index-state we can avoid recalculating
  # when new index-state-hashes are added.
  # See https://github.com/input-output-hk/haskell.nix/issues/672
  cached-index-state = if index-sha256 != null
    then index-state-found
    else
      let
        suitable-index-states =
          builtins.filter
            (s: s >= index-state-found) # This compare is why we need zulu time
            (builtins.attrNames index-state-hashes);
      in
        if builtins.length suitable-index-states == 0
          then index-state-found
          else pkgs.lib.head suitable-index-states;

  # Lookup hash for the index state we found
  index-sha256-found = if index-sha256 != null
    then index-sha256
    else index-state-hashes.${cached-index-state} or null;

in
  assert (if index-sha256-found == null
    then throw "Unknown index-state ${index-state-found}, the latest index-state I know about is ${pkgs.lib.last (builtins.attrNames index-state-hashes)}. You may need to update to a newer hackage.nix." else true);

let
  # Deal with source-repository-packages in a way that will work in
  # restricted-eval mode (as long as a sha256 is included).
  # Replace source-repository-package blocks that have a sha256 with
  # packages: block containing nix sotre paths of the fetched repos.
  replaceSoureRepos = projectFile:
    let
      blocks = pkgs.lib.splitString "\nsource-repository-package\n" ("\n" + projectFile);
      initialText = pkgs.lib.lists.take 1 blocks;
      repoBlocks = builtins.map (pkgs.haskell-nix.haskellLib.parseBlock cabalProjectFileName lookupSha256) (pkgs.lib.lists.drop 1 blocks);
      sourceRepos = pkgs.lib.lists.concatMap (x: x.sourceRepo) repoBlocks;
      otherText = pkgs.evalPackages.writeText "cabal.project" (pkgs.lib.strings.concatStringsSep "\n" (
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

  # The use of a the actual GHC can cause significant problems:
  # * For hydra to assemble a list of jobs from `components.tests` it must
  #   first have GHC that will be used. If a patch has been applied to the
  #   GHC to be used it must be rebuilt before the list of jobs can be assembled.
  #   If a lot of different GHCs are being tests that can be a lot of work all
  #   happening in the eval stage where little feedback is available.
  # * Once the jobs are running the compilation of the GHC needed (the eval
  #   stage already must have done it, but the outputs there are apparently
  #   not added to the cache) happens inside the IFD part of cabalProject.
  #   This causes a very large amount of work to be done in the IFD and our
  #   understanding is that this can cause problems on nix and/or hydra.
  # * When using cabalProject we cannot examine the properties of the project without
  #   building or downloading the GHC (less of an issue as we would normally need
  #   it soon anyway).
  #
  # The solution here is to capture the GHC outputs that `cabal v2-configure`
  # requests and materialize it so that the real GHC is only needed
  # when `checkMaterialization` is set.
  dummy-ghc-data = pkgs.haskell-nix.materialize ({
    sha256 = null;
    sha256Arg = "sha256";
    materialized = ../materialized/dummy-ghc + "/${ghc.targetPrefix}${ghc.name}-${pkgs.stdenv.buildPlatform.system}";
    reasonNotSafe = null;
  } // pkgs.lib.optionalAttrs (checkMaterialization != null) {
    inherit checkMaterialization;
  }) (
  runCommand ("dummy-data-" + ghc.name) {
    nativeBuildInputs = [ ghc ];
  } ''
    mkdir -p $out/ghc
    mkdir -p $out/ghc-pkg
    ${ghc.targetPrefix}ghc --version > $out/ghc/version
    ${ghc.targetPrefix}ghc --numeric-version > $out/ghc/numeric-version
    ${ghc.targetPrefix}ghc --info | grep -v /nix/store > $out/ghc/info
    ${ghc.targetPrefix}ghc --supported-languages > $out/ghc/supported-languages
    ${ghc.targetPrefix}ghc-pkg --version > $out/ghc-pkg/version
    # The order of the `ghc-pkg dump` output seems to be non
    # deterministic so we need to sort it so that it is always
    # the same.
    # Sort the output by spliting it on the --- separator line,
    # sorting it, adding the --- separators back and removing the
    # last line (the trailing ---)
    ${ghc.targetPrefix}ghc-pkg dump --global -v0 \
      | grep -v /nix/store \
      | grep -v '^abi:' \
      | tr '\n' '\r' \
      | sed -e 's/\r\r*/\r/g' \
      | sed -e 's/\r$//g' \
      | sed -e 's/\r---\r/\n/g' \
      | sort \
      | sed -e 's/$/\r---/g' \
      | tr '\r' '\n' \
      | sed -e '$ d' \
        > $out/ghc-pkg/dump-global
  '');

  # Dummy `ghc` that uses the captured output
  dummy-ghc = pkgs.evalPackages.writeTextFile {
    name = "dummy-" + ghc.name;
    executable = true;
    destination = "/bin/${ghc.targetPrefix}ghc";
    text = ''
      #!${pkgs.evalPackages.runtimeShell}
      case "$*" in
        --version*)
          cat ${dummy-ghc-data}/ghc/version
          ;;
        --numeric-version*)
          cat ${dummy-ghc-data}/ghc/numeric-version
          ;;
        --supported-languages*)
          cat ${dummy-ghc-data}/ghc/supported-languages
          ;;
        --print-global-package-db*)
          echo "$out/dumby-db"
          ;;
        --info*)
          cat ${dummy-ghc-data}/ghc/info
          ;;
        --print-libdir*)
          echo ${dummy-ghc-data}/ghc/libdir
          ;;
        *)
          echo "Unknown argument '$*'" >&2
          exit 1
          ;;
        esac
      exit 0
    '';
  };

  # Dummy `ghc-pkg` that uses the captured output
  dummy-ghc-pkg = pkgs.evalPackages.writeTextFile {
    name = "dummy-pkg-" + ghc.name;
    executable = true;
    destination = "/bin/${ghc.targetPrefix}ghc-pkg";
    text = ''
      #!${pkgs.evalPackages.runtimeShell}
      case "$*" in
        --version)
          cat ${dummy-ghc-data}/ghc-pkg/version
          ;;
        'dump --global -v0')
          cat ${dummy-ghc-data}/ghc-pkg/dump-global
          ;;
        *)
          echo "Unknown argument '$*'. " >&2
          echo "Additional ghc-pkg-options are not currently supported." >&2
          echo "See https://github.com/input-output-hk/haskell.nix/pull/658" >&2
          exit 1
          ;;
        esac
      exit 0
    '';
  };

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
  }) (pkgs.evalPackages.runCommand (if name == null then "plan-to-nix-pkgs" else name + "-plan-to-nix-pkgs") {
    nativeBuildInputs = [ nix-tools dummy-ghc dummy-ghc-pkg cabal-install pkgs.evalPackages.rsync ];
    # Needed or stack-to-nix will die on unicode inputs
    LOCALE_ARCHIVE = pkgs.lib.optionalString (pkgs.evalPackages.stdenv.buildPlatform.libc == "glibc") "${pkgs.evalPackages.glibcLocales}/lib/locale/locale-archive";
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
    # Decide what to do for each `package.yaml` file.
    for hpackFile in $(find . -name package.yaml); do (
      # Look to see if a `.cabal` file exists
      shopt -u nullglob
      for cabalFile in $(dirname $hpackFile)/*.cabal; do
        if [ -e "$cabalFile" ]; then
          echo Ignoring $hpackFile as $cabalFile exists
        else
          # warning: this may not generate the proper cabal file.
          # hpack allows globbing, and turns that into module lists
          # without the source available (we cleaneSourceWith'd it),
          # this may not produce the right result.
          echo No .cabal file found, running hpack on $hpackFile
          hpack $hpackFile
        fi
      done
      )
    done
    ${pkgs.lib.optionalString (subDir != "") "cd ${subDir}"}
    ${fixedProject.makeFixedProjectFile}
    ${pkgs.lib.optionalString (cabalProjectFreeze != null) ''
      cp ${pkgs.evalPackages.writeText "cabal.project.freeze" cabalProjectFreeze} \
        cabal.project.freeze
    ''}
    export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
    export GIT_SSL_CAINFO=${cacert}/etc/ssl/certs/ca-bundle.crt
    HOME=${
      # This creates `.cabal` directory that is as it would have
      # been at the time `cached-index-state`.  We may include
      # some packages that will be excluded by `index-state-found`
      # which is used by cabal (cached-index-state >= index-state-found).  
      dotCabal {
        inherit cabal-install nix-tools extra-hackage-tarballs;
        index-state = cached-index-state;
        sha256 = index-sha256-found;
      }
    } cabal v2-configure \
        --index-state=${
            # Setting the desired `index-state` here in case it was not
            # from the cabal.project file. This will further restrict the
            # packages used by the solver (cached-index-state >= index-state-found).
            builtins.trace ("Using index-state: ${index-state-found}" + (if name == null then "" else " for " + name))
              index-state-found} \
        --with-ghc=${ghc.targetPrefix}ghc \
        --with-ghc-pkg=${ghc.targetPrefix}ghc-pkg \
        --enable-tests \
        --enable-benchmarks \
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
    substituteInPlace $tmp${subDir'}/dist-newstyle/cache/plan.json --replace "$tmp" "$out"

    # run `plan-to-nix` in $out.  This should produce files right there with the
    # proper relative paths.
    (cd $out${subDir'} && plan-to-nix --full --plan-json $tmp${subDir'}/dist-newstyle/cache/plan.json -o .)

    # Remove the non nix files ".project" ".cabal" "package.yaml" files
    # as they should not be in the output hash (they may change slightly
    # without affecting the nix).
    if [ -d $out${subDir'}/.source-repository-packages ]; then
      chmod +w -R $out${subDir'}/.source-repository-packages
      rm -rf $out${subDir'}/.source-repository-packages
    fi
    find $out \( -type f -or -type l \) ! -name '*.nix' -delete
    # Remove empty dirs
    find $out -type d -empty -delete

    # move pkgs.nix to default.nix ensure we can just nix `import` the result.
    mv $out${subDir'}/pkgs.nix $out${subDir'}/default.nix
  '');
in {
  projectNix = plan-nix;
  index-state = index-state-found;
  inherit src;
  inherit (fixedProject) sourceRepos;
}
