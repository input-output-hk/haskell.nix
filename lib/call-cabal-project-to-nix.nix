{ pkgs, runCommand, cacert, index-state-hashes, haskellLib }:
{ name          ? src.name or null # optional name for better error messages
, src
, materialized-dir ? ../materialized
, compiler-nix-name    # The name of the ghc compiler to use eg. "ghc884"
, index-state   ? null # Hackage index-state, eg. "2019-10-10T00:00:00Z"
, index-sha256  ? null # The hash of the truncated hackage index-state
, plan-sha256   ? null # The hash of the plan-to-nix output (makes the plan-to-nix step a fixed output derivation)
, materialized  ? null # Location of a materialized copy of the nix files
, checkMaterialization ? null # If true the nix files will be generated used to check plan-sha256 and material
, cabalProjectFileName ? "cabal.project"
, cabalProject         ? null
, cabalProjectLocal    ? null
, cabalProjectFreeze   ? null
, caller               ? "callCabalProjectToNix" # Name of the calling function for better warning messages
, compilerSelection    ? p: p.haskell-nix.compiler
, ghc           ? null # Deprecated in favour of `compiler-nix-name`
, ghcOverride   ? null # Used when we need to set ghc explicitly during bootstrapping
, configureArgs ? "" # Extra arguments to pass to `cabal v2-configure`.
                     # `--enable-tests --enable-benchmarks` are included by default.
                     # If the tests and benchmarks are not needed and they
                     # cause the wrong plan to be chosen, then we can use
                     # `configureArgs = "--disable-tests --disable-benchmarks";`
, sha256map     ? null
                     # An alternative to adding `--sha256` comments into the
                     # cabal.project file:
                     #   sha256map =
                     #     { # For a `source-repository-package` use the `location` and `tag` as the key
                     #       "https://github.com/jgm/pandoc-citeproc"."0.17"
                     #         = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q";
                     #       # For a `repository` use the `url` as the key
                     #       "https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/bfc363b9f879c360e0a0460ec0c18ec87222ec32"
                     #         = "sha256-g9xGgJqYmiczjxjQ5JOiK5KUUps+9+nlNGI/0SpSOpg=";
                     #     };
, inputMap ? {}
                     # An alternative to providing a `sha256` handy for flakes
                     # cabal.project file:
                     #   inputs.pandoc-citeproc.url = "github:jgm/pandoc-citeproc/0.17";
                     #   inputs.pandoc-citeproc.flake = false;
                     #   outputs = inputs:
                     #     ...
                     #     inputMap."https://github.com/jgm/pandoc-citeproc" = inputs.inputs.pandoc-citeproc;
, extra-hackage-tarballs ? {}
, source-repo-override ? {} # Cabal seems to behave incoherently when
                            # two source-repository-package entries
                            # provide the same packages, making it
                            # impossible to override cabal.project
                            # with e.g. a cabal.project.local. In CI,
                            # we want to be able to test against the
                            # latest versions of various dependencies.
                            #
                            # This argument is a map from url to
                            # a function taking the existing repoData
                            # and returning the new repoData in its
                            # place. E.g.
                            #
                            # { "https://github.com/input-output-hk/plutus-apps" = orig: orig // { subdirs = (orig.subdirs or [ "." ]) ++ [ "foo" ]; }; }
                            #
                            # would result in the "foo" subdirectory of
                            # any plutus-apps input being used for a
                            # package.
, evalPackages
, supportHpack ? false      # Run hpack on package.yaml files with no .cabal file
, ignorePackageYaml ? false # Ignore package.yaml files even if they exist
, ...
}@args:
let
  inherit (evalPackages.haskell-nix) materialize dotCabal;

  # These defaults are here rather than in modules/cabal-project.nix to make them
  # lazy enough to avoid infinite recursion issues.
  # Using null as the default also improves performance as they are not forced by the
  # nix module system for `nix-tools-unchecked` and `cabal-install-unchecked`.
  nix-tools = if args.nix-tools or null != null
    then args.nix-tools
    else evalPackages.haskell-nix.nix-tools-unchecked;
  cabal-install = nix-tools.exes.cabal;
  forName = pkgs.lib.optionalString (name != null) (" for " + name);
  nameAndSuffix = suffix: if name == null then suffix else name + "-" + suffix;

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
              (compilerSelection pkgs)."${compiler-nix-name}";

in let
  ghc = ghc';
  subDir' = src.origSubDir or "";
  subDir = pkgs.lib.strings.removePrefix "/" subDir';

  cleanedSource = haskellLib.cleanSourceWith {
    name = if name != null then "${name}-root-cabal-files" else "source-root-cabal-files";
    src = src.origSrc or src;
    filter = path: type: (!(src ? filter) || src.filter path type) && (
      type == "directory" ||
      pkgs.lib.any (i: (pkgs.lib.hasSuffix i path)) [ ".cabal" "package.yaml" ]); };

  # When there is no `cabal.project` file `cabal-install` behaves as if there was
  # one containing `packages: ./*.cabal`.  Even if there is a `cabal.project.local`
  # containing some other `packages:`, it still includes `./*.cabal`.
  #
  # We could write to `cabal.project.local` instead of `cabal.project` when
  # `cabalProject == null`.  However then `cabal-install` will look in parent
  # directories for a `cabal.project` file. That would complicate reasoning about
  # the relative directories of packages.
  #
  # Instead we treat `cabalProject == null` as if it was `packages: ./*.cabal`.
  #
  # See: https://github.com/input-output-hk/haskell.nix/pull/1588
  #      https://github.com/input-output-hk/haskell.nix/pull/1639
  #
  rawCabalProject = ''
    ${
      if cabalProject == null
        then ''
          -- Included to match the implicit project used by `cabal-install`
          packages: ./*.cabal
        ''
        else cabalProject
    }
    ${
      pkgs.lib.optionalString (cabalProjectLocal != null) ''
        -- Added from `cabalProjectLocal` argument to the `cabalProject` function
        ${cabalProjectLocal}
      ''
    }
  '';

  index-state-max =
    if index-state != null
    then index-state
    else pkgs.lib.last (builtins.attrNames index-state-hashes);

  # If a hash was not specified find a suitable cached index state to
  # use that will contain all the packages we need.  By using the
  # first one after the desired index-state we can avoid recalculating
  # when new index-state-hashes are added.
  # See https://github.com/input-output-hk/haskell.nix/issues/672
  cached-index-state = if index-sha256 != null
    then index-state-max
    else
      let
        suitable-index-states =
          builtins.filter
            (s: s >= index-state-max) # This compare is why we need zulu time
            (builtins.attrNames index-state-hashes);
      in
        if builtins.length suitable-index-states == 0
          then index-state-max
          else pkgs.lib.head suitable-index-states;

  # Lookup hash for the index state we found
  index-sha256-found = if index-sha256 != null
    then index-sha256
    else index-state-hashes.${cached-index-state} or null;

in
  assert (if index-sha256-found == null
    then throw "Unknown index-state ${index-state-max}, the latest index-state I know about is ${pkgs.lib.last (builtins.attrNames index-state-hashes)}. You may need to update to a newer hackage.nix." else true);

let
  # Deal with source-repository-packages in a way that will work in
  # restricted-eval mode (as long as a sha256 is included).
  # Replace source-repository-package blocks that have a sha256 with
  # packages: block containing nix store paths of the fetched repos.

  hashPath = path:
    builtins.readFile (pkgs.runCommand "hash-path" { preferLocalBuild = true; }
      "echo -n $(${pkgs.nix}/bin/nix-hash --type sha256 --base32 ${path}) > $out");

  replaceSourceRepos = projectFile:
    let
      fetchPackageRepo = fetchgit: repoData:
        let
          fetched =
            if inputMap ? "${repoData.url}/${repoData.rev or repoData.ref}"
              then inputMap."${repoData.url}/${repoData.rev or repoData.ref}"
            else if inputMap ? ${repoData.url}
              then
                (if inputMap.${repoData.url}.rev != (repoData.rev or repoData.ref)
                  then throw "${inputMap.${repoData.url}.rev} may not match ${repoData.rev or repoData.ref} for ${repoData.url} use \"${repoData.url}/${repoData.rev or repoData.ref}\" as the inputMap key if ${repoData.rev or repoData.ref} is a branch or tag that points to ${inputMap.${repoData.url}.rev}."
                  else inputMap.${repoData.url})
            else if repoData.sha256 != null
            then fetchgit { inherit (repoData) url sha256; rev = repoData.rev or repoData.ref; }
            else
              let drv = builtins.fetchGit
                { inherit (repoData) url ; ref = repoData.ref or null; }
                # fetchGit does not accept "null" as rev, so when it's null
                # we have to omit the argument completely.
                // pkgs.lib.optionalAttrs (repoData ? rev) { inherit (repoData) rev; };
              in __trace "WARNING: No sha256 found for source-repository-package ${repoData.url} ref=${repoData.ref or "(unspecified)"} rev=${repoData.rev or "(unspecified)"} download may fail in restricted mode (hydra)"
                (__trace "Consider adding `--sha256: ${hashPath drv}` to the ${cabalProjectFileName} file or passing in a sha256map argument"
                 drv);
        in {
          # Download the source-repository-package commit and add it to a minimal git
          # repository that `cabal` will be able to access from a non fixed output derivation.
          location = evalPackages.runCommand "source-repository-package" {
              nativeBuildInputs = [ evalPackages.rsync evalPackages.gitMinimal ];
            } ''
            mkdir $out
            rsync -a --prune-empty-dirs "${fetched}/" "$out/"
            cd $out
            chmod -R +w .
            git init -b minimal
            git add --force .
            GIT_COMMITTER_NAME='No One' GIT_COMMITTER_EMAIL= git commit -m "Minimal Repo For Haskell.Nix" --author 'No One <>'
          '';
          inherit (repoData) subdirs;
          inherit fetched;
          tag = "minimal";
        };

      # Parse the `source-repository-package` blocks
      sourceRepoPackageResult = pkgs.haskell-nix.haskellLib.parseSourceRepositoryPackages
        cabalProjectFileName sha256map source-repo-override projectFile;

      # Parse the `repository` blocks
      repoResult = pkgs.haskell-nix.haskellLib.parseRepositories
        evalPackages cabalProjectFileName sha256map inputMap cabal-install nix-tools sourceRepoPackageResult.otherText;

      # we need the repository content twice:
      # * at eval time (below to build the fixed project file)
      #   Here we want to use evalPackages.fetchgit, so one can calculate
      #   the build plan for any target without a remote builder
      # * at built time  (passed out)
      #   Here we want to use plain pkgs.fetchgit, which is what a builder
      #   on the target system would use, so that the derivation is unaffected
      #   and, say, a linux release build job can identify the derivation
      #   as built by a darwin builder, and fetch it from a cache
      sourceReposEval = builtins.map (fetchPackageRepo evalPackages.fetchgit) sourceRepoPackageResult.sourceRepos;
      sourceReposBuild = builtins.map (x: (fetchPackageRepo pkgs.fetchgit x).fetched) sourceRepoPackageResult.sourceRepos;
    in {
      sourceRepos = sourceReposBuild;
      inherit (repoResult) repos extra-hackages;
      makeFixedProjectFile = ''
        cp -f ${evalPackages.writeText "cabal.project" sourceRepoPackageResult.otherText} ./cabal.project
      '' +
        pkgs.lib.optionalString (builtins.length sourceReposEval != 0) (''
        chmod +w -R ./cabal.project
        # The newline here is important in case cabal.project does not have one at the end
        echo >> ./cabal.project
      '' +
        # Add replacement `source-repository-package` blocks pointing to the minimal git repos
        ( pkgs.lib.strings.concatMapStrings (f: ''
              echo "source-repository-package" >> ./cabal.project
              echo "  type: git" >> ./cabal.project
              echo "  location: file://${f.location}" >> ./cabal.project
              echo "  subdir: ${builtins.concatStringsSep " " f.subdirs}" >> ./cabal.project
              echo "  tag: ${f.tag}" >> ./cabal.project
            '') sourceReposEval
        ));
      # This will be used to replace refernces to the minimal git repos with just the index
      # of the repo.  The index will be used in lib/import-and-filter-project.nix to
      # lookup the correct repository in `sourceReposBuild`.  This avoids having
      # `/nix/store` paths in the `plan-nix` output so that it can  be materialized safely.
      replaceLocations = pkgs.lib.strings.concatStrings (
            pkgs.lib.lists.zipListsWith (n: f: ''
              (cd $out${subDir'}
              substituteInPlace $tmp${subDir'}/dist-newstyle/cache/plan.json --replace file://${f.location} ${builtins.toString n}
              for a in $(grep -rl file://${f.location} .plan.nix/*.nix); do
                substituteInPlace $a --replace file://${f.location} ${builtins.toString n}
              done)
            '')
              (pkgs.lib.lists.range 0 ((builtins.length sourceReposEval) - 1))
              sourceReposEval
          );
    };

  fixedProject = replaceSourceRepos rawCabalProject;

  inherit (ghc) dummy-ghc-data;

  # Dummy `ghc` that uses the captured output
  dummy-ghc = evalPackages.writeTextFile {
    name = "dummy-" + ghc.name;
    executable = true;
    destination = "/bin/${ghc.targetPrefix}ghc";
    text = ''
      #!${evalPackages.runtimeShell}
      case "$*" in
        --version*)
          cat ${dummy-ghc-data}/ghc/version
          ;;
        --numeric-version*)
          cat ${dummy-ghc-data}/ghc/numeric-version
          ;;
      ${pkgs.lib.optionalString (ghc.targetPrefix == "js-unknown-ghcjs-") ''
        --numeric-ghc-version*)
          cat ${dummy-ghc-data}/ghc/numeric-ghc-version
          ;;
        --numeric-ghcjs-version*)
          cat ${dummy-ghc-data}/ghc/numeric-ghcjs-version
          ;;
      ''}
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
  dummy-ghc-pkg = evalPackages.writeTextFile {
    name = "dummy-pkg-" + ghc.name;
    executable = true;
    destination = "/bin/${ghc.targetPrefix}ghc-pkg";
    text = ''
      #!${evalPackages.runtimeShell}
      case "$*" in
        --version)
          cat ${dummy-ghc-data}/ghc-pkg/version
          ;;
      ${pkgs.lib.optionalString (ghc.targetPrefix == "js-unknown-ghcjs-") ''
        --numeric-ghcjs-version)
          cat ${dummy-ghc-data}/ghc-pkg/numeric-ghcjs-version
          ;;
      ''}
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
    this = "project.plan-nix" + (if name != null then " for ${name}" else "");
  } // pkgs.lib.optionalAttrs (checkMaterialization != null) {
    inherit checkMaterialization;
  }) (evalPackages.runCommand (nameAndSuffix "plan-to-nix-pkgs") {
    nativeBuildInputs =
      # The things needed from nix-tools
      [ nix-tools.exes.make-install-plan
        nix-tools.exes.plan-to-nix
        nix-tools.exes.cabal
      ]
      ++ pkgs.lib.optional supportHpack nix-tools.exes.hpack
      ++ [dummy-ghc dummy-ghc-pkg evalPackages.rsync evalPackages.gitMinimal evalPackages.allPkgConfigWrapper ];
    # Needed or stack-to-nix will die on unicode inputs
    LOCALE_ARCHIVE = pkgs.lib.optionalString (evalPackages.stdenv.buildPlatform.libc == "glibc") "${evalPackages.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";
    meta.platforms = pkgs.lib.platforms.all;
    preferLocalBuild = false;
    outputs = [
      "out"           # The results of plan-to-nix
      # These two output will be present if in cabal configure failed.
      # They are used to provide passthru.json and passthru.freeze that
      # check first for cabal configure failure.
      "json"    # The `plan.json` file generated by cabal and used for `plan-to-nix` input
      "freeze"  # The `cabal.project.freeze` file created by `cabal v2-freeze`
    ];
  } ''
    tmp=$(mktemp -d)
    cd $tmp
    # if cleanedSource is empty, this means it's a new
    # project where the files haven't been added to the git
    # repo yet. We fail early and provide a useful error
    # message to prevent headaches (#290).
    if [ -z "$(ls -A ${cleanedSource})" ]; then
      echo "cleaned source is empty. Did you forget to 'git add -A'?"
      ${pkgs.lib.optionalString (__length fixedProject.sourceRepos == 0) ''
        exit 1
      ''}
    else
      rsync -a ${cleanedSource}/ ./
    fi
    chmod +w -R .
    # Decide what to do for each `package.yaml` file.
    for hpackFile in $(find . -name package.yaml); do (
      # Look to see if a `.cabal` file exists
      shopt -u nullglob
      for cabalFile in $(dirname $hpackFile)/*.cabal; do
        if [ -e "$cabalFile" ]; then
          echo Ignoring $hpackFile as $cabalFile exists
        else
          ${
          # warning: this may not generate the proper cabal file.
          # hpack allows globbing, and turns that into module lists
          # without the source available (we cleanSourceWith'd it),
          # this may not produce the right result.
          if supportHpack
            then '' 
              echo No .cabal file found, running hpack on $hpackFile
              hpack $hpackFile
            ''
            else ''
              echo "WARNING $hpackFile has no .cabal file and \`supportHpack\` was not set."
            ''
          }
        fi
      done
      )
    done
    ${pkgs.lib.optionalString (subDir != "") "cd ${subDir}"}
    ${fixedProject.makeFixedProjectFile}
    ${pkgs.lib.optionalString (cabalProjectFreeze != null) ''
      cp ${evalPackages.writeText "cabal.project.freeze" cabalProjectFreeze} \
        cabal.project.freeze
      chmod +w cabal.project.freeze
    ''}
    export SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt
    export GIT_SSL_CAINFO=${cacert}/etc/ssl/certs/ca-bundle.crt

    CABAL_DIR=${
      # This creates `.cabal` directory that is as it would have
      # been at the time `cached-index-state`.  We may include
      # some packages that will be excluded by `index-state-max`
      # which is used by cabal (cached-index-state >= index-state-max).
      dotCabal {
        inherit cabal-install nix-tools extra-hackage-tarballs;
        extra-hackage-repos = fixedProject.repos;
        index-state = cached-index-state;
        sha256 = index-sha256-found;
      }
    } make-install-plan ${
          # Setting the desired `index-state` here in case it is not
          # in the cabal.project file. This will further restrict the
          # packages used by the solver (cached-index-state >= index-state-max).
          pkgs.lib.optionalString (index-state != null) "--index-state=${index-state}"
        } \
        -w ${
          # We are using `-w` rather than `--with-ghc` here to override
          # the `with-compiler:` in the `cabal.project` file.
          ghc.targetPrefix}ghc \
        --with-ghc-pkg=${ghc.targetPrefix}ghc-pkg \
        --enable-tests \
        --enable-benchmarks \
        ${pkgs.lib.optionalString (ghc.targetPrefix == "js-unknown-ghcjs-")
            "--ghcjs --with-ghcjs=js-unknown-ghcjs-ghc --with-ghcjs-pkg=js-unknown-ghcjs-ghc-pkg"} \
        ${configureArgs}

    mkdir -p $out

    cp cabal.project.freeze $freeze
    # Not needed any more (we don't want it to wind up in the $out hash)
    rm cabal.project.freeze

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

    # Make sure the subDir' exists even if it did not contain any cabal files
    mkdir -p $out${subDir'}

    # make sure the path's in the plan.json are relative to $out instead of $tmp
    # this is necessary so that plan-to-nix relative path logic can work.
    substituteInPlace $tmp${subDir'}/dist-newstyle/cache/plan.json --replace "$tmp" "$out"

    # run `plan-to-nix` in $out.  This should produce files right there with the
    # proper relative paths.
    (cd $out${subDir'} && plan-to-nix --full ${if ignorePackageYaml then "--ignore-package-yaml" else ""} --plan-json $tmp${subDir'}/dist-newstyle/cache/plan.json -o .)

    # Replace the /nix/store paths to minimal git repos with indexes (that will work with materialization).
    ${fixedProject.replaceLocations}

    # Make the plan.json file available in case we need to debug plan-to-nix
    cp $tmp${subDir'}/dist-newstyle/cache/plan.json $json

    # Remove the non nix files ".project" ".cabal" "package.yaml" files
    # as they should not be in the output hash (they may change slightly
    # without affecting the nix).
    find $out \( -type f -or -type l \) ! -name '*.nix' -delete

    # Make the revised cabal files available (after the delete step avove)
    echo "Moving cabal files from $tmp${subDir'}/dist-newstyle/cabal-files to $out${subDir'}/cabal-files"
    mv $tmp${subDir'}/dist-newstyle/cabal-files $out${subDir'}/cabal-files

    # Remove empty dirs
    find $out -type d -empty -delete

    # move pkgs.nix to default.nix ensure we can just nix `import` the result.
    mv $out${subDir'}/pkgs.nix $out${subDir'}/default.nix
  '');
in {
  projectNix = plan-nix;
  inherit index-state-max src;
  inherit (fixedProject) sourceRepos extra-hackages;
}
