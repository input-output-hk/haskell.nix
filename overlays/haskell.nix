{ sources }:
# The haskell.nix infrastructure
#
# for hygienic reasons we'll use haskell-nix as a prefix.
# Using haskell.nix in nix is awkward as I needs to be quoted.
final: prev: {
    haskell-nix = with final.haskell-nix; {

        # Default modules, these will always be included.
        # They are here to be overridden/added to by other
        # overlays.
        defaultModules = prev.haskell-nix.defaultModules or [];

        # Additional user-provided mappings to augment ./../lib/pkgconf-nixpkgs-map.nix
        extraPkgconfigMappings = prev.haskell-nix.extraPkgconfigMappings or {};

        # Nix Flake based source pins.
        # To update all inputs, get unstable Nix and then `nix flake update --recreate-lock-file`
        # Or `nix-shell -p nixVersions.latest --run "nix --experimental-features 'nix-command flakes' flake update --recreate-lock-file"`
        sources = sources;

        # We provide a `callPackage` function to consumers for
        # convenience.  We will however refrain from using it
        # here and be explicit about imports and dependencies.
        callPackage = prev.lib.callPackageWith (final // final.haskell-nix);

        # You can provide different pins for hackage.nix and stackage.nix if required.
        # It's also possible to override these sources with NIX_PATH.
        hackageSourceJSON = ../hackage-src.json;
        stackageSourceJSON = ../stackage-src.json;

        # ghc hackage patches.
        # these are patches that turn hackage packages into the same as the ones
        # ghc ships with the supposedly same version. See GHC Track Issue: 16199
        ghcHackagePatches = import ../patches;

        compat = import ../lib/compat.nix;

        # Utility function for downloading a pinned git repo, that can be
        # overridden with NIX_PATH.
        fetchExternal = import ../lib/fetch-external.nix;

        # Functions for cleaning Haskell source directories.
        inherit (import ../lib/clean-source-haskell.nix { inherit (final) lib; })
          haskellSourceFilter
          cleanSourceHaskell;

        # All packages from Hackage as Nix expressions
        hackageSrc = sources.hackage;
        hackage = import hackageSrc;

        # Contains the hashes of the cabal 01-index.tar.gz for given
        # index states.  Starting from April 1st 2019.
        indexStateHashesPath = hackageSrc + "/index-state-hashes.nix";

        # The set of all Stackage snapshots
        stackageSrc = sources.stackage;
        stackage = import stackageSrc;

        # Utility functions for working with the component builder.
        haskellLib = let hl = import ../lib {
            pkgs = final;
            inherit (final) stdenv lib recurseIntoAttrs srcOnly;
            haskellLib = hl;
        }; in hl;

        # Create a Haskell package set based on a cabal build plan (plan-to-nix)
        # and Nix expressions representing cabal packages (cabal-to-nix).
        mkPkgSet =
            { pkg-def  # Base package set. Either from stackage (via stack-to-nix) or from a cabal projects plan file (via plan-to-nix)
            , pkg-def-extras ? [] # Additional packages to augment the Base package set `pkg-def` with.
            , modules ? []
            , extra-hackages ? [] # Extra Hackage repositories to use besides main one.
            }@args:

            let
              hackageAll = builtins.foldl' final.lib.recursiveUpdate hackage extra-hackages;
            in

            import ../package-set.nix {
                inherit (args) pkg-def pkg-def-extras;
                modules = defaultModules ++ modules;
                pkgs = final;
                hackage = hackageAll;
            };

        # Some boot packages (libiserv) are in lts, but not in hackage,
        # so we should not try to get it from hackage based on the stackage
        # info.  Instead we can add ghc-boot-packages to `pkg-def-extras`.
        # The compiler-nix-name allows non default values (like
        # "ghc8102-experimental").
        excludeBootPackages = compiler-nix-name: pkg-def: hackage:
          let original = pkg-def hackage;
              bootPkgNames = final.lib.attrNames
                final.ghc-boot-packages.${
                  if compiler-nix-name != null
                    then compiler-nix-name
                    else (pkg-def hackage).compiler.nix-name};
          in original // {
            packages = final.lib.filterAttrs (n: _: final.lib.all (b: n != b) bootPkgNames)
              original.packages;
          };

        # Create a Haskell package set based on a Stack configuration.
        mkStackPkgSet =
            { stack-pkgs  # Path to the output of stack-to-nix
            , pkg-def-extras ? []
            , modules ? []
            }:
            let
                # The Stackage release referenced in the stack config
                pkg-def = stackage.${stack-pkgs.resolver} or (throw ''
                This version of stackage.nix does not know about the Stackage resolver ${stack-pkgs.resolver}.
                You may need to update haskell.nix to one that includes a newer stackage.nix.
                '');
                # The compiler referenced in the stack config
                compiler = (stack-pkgs.extras hackage).compiler or (pkg-def hackage).compiler;
                patchesModule = ghcHackagePatches.${compiler.nix-name} or {};
                # Remove fake packages generated from stack keywords used in ghc-options
                removeStackSpecial = module: if builtins.typeOf module == "set"
                  then module // { packages = removeSpecialPackages (module.packages or {}); }
                  else module;
                removeSpecialPackages = ps: removeAttrs ps [ "$locals" "$targets" "$everything" ];
            in mkPkgSet {
                pkg-def = excludeBootPackages compiler.nix-name pkg-def;
                pkg-def-extras = [ stack-pkgs.extras
                                   final.ghc-boot-packages.${compiler.nix-name}
                                 ]
                              ++ pkg-def-extras;
                # set doExactConfig = true. The stackage set should be consistent
                # and we should trust stackage here!
                modules = [ { doExactConfig = true; } patchesModule ]
                       ++ modules
                       ++ map removeStackSpecial (stack-pkgs.modules or []);
            };

        # Create a Haskell package set based on a Cabal configuration.
        mkCabalProjectPkgSet =
            { plan-pkgs  # Path to the output of plan-to-nix
            , pkg-def-extras ? []
            , modules ? []
            , extra-hackages ? []
            , compiler-nix-name ? null
            , compilerSelection ? p: p.haskell-nix.compiler
            }:

            let
                compiler-nix-name' =
                  if compiler-nix-name != null
                    then compiler-nix-name
                    else ((plan-pkgs.extras hackage).compiler or (plan-pkgs.pkgs hackage).compiler).nix-name;
                pkg-def = excludeBootPackages compiler-nix-name plan-pkgs.pkgs;
                patchesModule = ghcHackagePatches.${compiler-nix-name'} or {};
                package.compiler-nix-name.version = (compilerSelection final.buildPackages).${compiler-nix-name'}.version;
                withMsg = final.lib.assertMsg;
            in
              mkPkgSet {
                inherit pkg-def;
                pkg-def-extras = [ plan-pkgs.extras ]
                             ++ pkg-def-extras;
                # set doExactConfig = true, as we trust cabals resolution for
                # the plan.
                modules = [ { doExactConfig = true; } patchesModule ]
                       ++ modules
                       ++ plan-pkgs.modules or [];
                inherit extra-hackages;
            };

        # Package sets for all stackage snapshots.
        snapshots = import ../snapshots.nix { inherit (final) lib ghc-boot-packages; inherit mkPkgSet stackage excludeBootPackages; };
        # Pick a recent LTS snapshot to be our "default" package set.
        haskellPackages =
            if final.stdenv.targetPlatform.isAarch64 && final.stdenv.buildPlatform.isAarch64
            then snapshots."lts-15.13"
            else snapshots."lts-14.13";

        # Creates Cabal local repository from { name, index } set.
        mkLocalHackageRepo = import ../mk-local-hackage-repo final;

        # Dummy version of ghc to work around https://github.com/haskell/cabal/issues/8352
        cabal-issue-8352-workaround = [
          (final.writeTextFile {
            name = "dummy-ghc";
            executable = true;
            destination = "/bin/ghc";
            text = ''
              #!${final.runtimeShell}
              case "$*" in
                --version*)
                  echo 'The Glorious Glasgow Haskell Compilation System, version 8.10.7'
                  ;;
                --numeric-version*)
                  echo '8.10.7'
                  ;;
                --supported-languages*)
                  echo Haskell2010
                  ;;
                --info*)
                  echo '[]'
                  ;;
                *)
                  echo "Unknown argument '$*'" >&2
                  exit 1
                  ;;
              esac
              exit 0
            '';
          })
          (final.writeTextFile {
            name = "dummy-ghc";
            executable = true;
            destination = "/bin/ghc-pkg";
            text = ''
              #!${final.runtimeShell}
              case "$*" in
                --version*)
                  echo 'GHC package manager version 8.10.7'
                  ;;
                *)
                  echo "Unknown argument '$*'" >&2
                  exit 1
                  ;;
              esac
              exit 0
            '';
          })
        ];

        dotCabal = { index-state, sha256, extra-hackage-tarballs ? {}, extra-hackage-repos ? {}, nix-tools, ... }:
            let
              # NOTE: root-keys: aaa is because key-threshold: 0 does not seem to be enough by itself
              bootstrapIndexTarball = name: index: final.runCommand "cabal-bootstrap-index-tarball-${name}" {
                nativeBuildInputs = [ nix-tools.exes.cabal ] ++ cabal-issue-8352-workaround;
              } ''
                HOME=$(mktemp -d)
                mkdir -p $HOME/.cabal/packages/${name}
                cat <<EOF > $HOME/.cabal/config
                repository ${name}
                  url: file:${mkLocalHackageRepo { inherit name index; }}
                  secure: True
                  root-keys: aaa
                  key-threshold: 0
                EOF
                cabal v2-update ${name}
                cp -r $HOME/.cabal/packages/${name} $out
                '';

              # Produce a fixed output derivation from a moving target (hackage index tarball)
              bootstrapped-hackage-tarball =
                bootstrapIndexTarball "hackage.haskell.org"
                (final.fetchurl {
                  name = "01-index.tar.gz-at-${builtins.replaceStrings [ ":" ] [ "" ] index-state}";
                  url = "https://hackage.haskell.org/01-index.tar.gz";
                  downloadToTemp = true;
                  postFetch = "${nix-tools}/bin/truncate-index -o $out -i $downloadedFile -s ${index-state}";
                  outputHashAlgo = "sha256";
                  outputHash = sha256;
                });

              bootstrapped-extra-hackage-tarballs = final.lib.mapAttrs bootstrapIndexTarball extra-hackage-tarballs;
            in
              # dotCabal creates a suitable CABAL_DIR for cabal to use to make an install plan.
              # This directory will need to include two things:
              #
              # 1) pre-downloaded and pre-bootstrapped repositories
              # 2) a configuration file
              #
              # NOTE: both steps need to be completed exactly as cabal would complete them. We won't
              # be able to alter CABAL_DIR at all after this since it will be stored in the nix store.
              # If these steps are not done properly, few things could go wrong, e.g.:
              #
              # - if cabal.config is missing cabal will try to write a default configuration file and
              #   fail with "Permission denied"
              # - if 01-index.tar.idx is missing cabal will fail to read the index and error out with
              #   "Could not read index. Did you call 'checkForUpdates'?"
              # - cabal will try to recreate 01-index.cache on the nix store and fail with "Permission
              #   denied"
              #
              # Let's examine the steps above one by one.
              #
              # Step 1) is typically the result of calling `cabal update`. Because Haskell.nix supports
              # different ways of including extra repositories, we need to divide this step into two
              # other steps.
              #
              # 1a) Download the index tarball (01-index.tar.gz) and the TUF matadata (mirrors.json,
              #     root.json, snapshot.json, timestamp.json)
              # 1b) Decompress the index tarball and create additional index/cache files (01-index.cache,
              #     01-index.tar, 01-index.tar.idx, 01-index.timestamp)
              #
              # dotCabal supports pre-populating a CABAL_DIR from two kind of repositories:
              #
              # - extra-hackage-repos
              # - extra-hackage-tarballs
              #
              # These repositories are in addititon to hackage, which is always prepopulated.
              #
              # NOTE: this might not be 100% correct since cabal can work without hackage being defined
              # in the global configuration at all. As a workaround, a project that does not want to use
              # hackage can use an explicit `active-repositories:` in the project configuration.
              # Haskell.nix will prepopulate hackage in CABAL_DIR but then cabal will not use it for
              # project planning.
              #
              # Let's examine how we deal with these repositories.
              #
              # - hackage: Hackage index tarball is downloaded and truncated from hackage.haskell.org.
              #   Since this is only the tarball, we need to add the TUF files and we need to bootstrap
              #   it (both part of bootstrapIndexTarball called above). Additionally, cabal will always
              #   include hackage when creating a default global configuration, so we need to add it to
              #   the global cabal config as well.
              #
              # - extra-hackage-repos: The repos are parsed from the project configuration but the user
              #   is responsible for downloading the whole repo (not only the tarball but also the TUF
              #   files). The downloaded repository still needs to be bootstrapped but it's done in
              #   ./lib/cabal-project-parser.nix, before we get here. We don't need to add these
              #   repositories in cabal.config since they are already present in the project configuration.
              #
              # - extra-hackage-tarballs: These are index tarballs the user is asking haskell.nix to
              #   "inject" into the project. They work in a similar way to hackage, since they also need
              #   TUF files, bootstrap and to be added in the cabal configuration.
              #
              #                        |           |                 | needs to be added |
              #                        | needs TUF | needs bootstrap | to cabal.config   |
              # -----------------------+-----------+-----------------+-------------------+
              # hackage                | yes       | yes             | yes               |
              # extra-hackage-repos    | no        | already done    | no                |
              # extra-hackage-tarballs | yes       | yes             | yes               |
              # -----------------------+-----------+-----------------+-------------------+
              #
              # Step 2) is writing the global cabal config. cabal writes a default global configuration
              # file at any invocation if it is missing. This is a simple step but there is one trick
              # we need to be aware and careful about. cabal populates CABAL_DIR with repositories obtained
              # from somewhere (the repository's url). What we are doing here is prepopulating CABAL_DIR
              # for cabal, we do not want to change where cabal thinks a repository is coming from.
              #
              # E.g. after prepopulating CABAL_DIR/packages/hackage.haskell.org from the nix path
              # /nix/store/ff4pn0yva7ndsrg2zshy8qxzlrfsr4cl-cabal-bootstrap-index-tarball-hackage.haskell.org/
              # the configuration we want to write is still
              #
              #   repository hackage.haskell.org
              #     url: http://hackage.haskell.org
              #     secure: True
              #
              # and not
              #
              #   repository hackage.haskell.org
              #     url: file:/nix/store/ff4pn0yva7ndsrg2zshy8qxzlrfsr4cl-cabal-bootstrap-index-tarball-hackage.haskell.org/
              #     secure: True
              #
              # Doing this correctly is important for few reasons:
              #
              # 1. cabal as called by haskell.nix will produce exactly the same install plan as cabal
              #    called outside of haskell.nix (other things equal, of course)
              # 2. The url of the repositories will be visible in the install plan. Having the correct
              #    urls there will allow us to know from where to fetch the packages tarballs at build
              #    time.
              # 3. We don't want to leak the nix path of the index into the derivation of the component
              #    builder since this will cause unnecessary recompilation. In other words, the recipe to
              #    compile a package has to only depend on its content, not on where the recipe is from
              #    or how it is obtained.
              #
              # Using the correct url for the repository is only possible if the url is well-known. Again
              # there is a difference in this regard among the different kind of repositories.
              #
              # - hackage: hackage's url is always well known.
              # - extra-hackage-repos: since the user is responsible for downloading these repositories,
              #   we don't really know where they are from, BUT, we also don't have to include them in
              #   cabal.config because the user has already included them in the project configuration.
              # - extra-hackage-tarballs: this is just a tarball passed to haskell.nix, we have no idea
              #   if the corresponding repository is published anywhere. The best we can do in this case
              #   is to form a url from the key of the extra-hackage-tarball and leave the user to decide.
              #   The user could use a key corresponding to a real reository domain or overwrite the
              #   packages source manually.
              #   E.g. passing
              #
              #     extra-hackage-tarballs = { extra-hackage-demo = ./01-index.tar.gz; };
              #
              #   will produce in cabal.config
              #
              #     repository extra-hackage-demo
              #       url: http://extra-hackage-demo/
              #       secure: True
              #
              #   and any package from that index will have a source url like
              #
              #   http://extra-hackage-demo/package/external-package-demo-0.1.0.0.tar.gz
              #
              #   If "extra-hackage-demo" is not a real domain, the user can correct those source urls
              #   while calling cabalProject
              #
              #   modules = [
              #     { packages.external-package-demo.src = demo-src; }
              #   ];
              #
              # In summary:
              #                        | well know url | what to do |
              # -----------------------+---------------+------------+
              # hackage                | yes           | use it     |
              # extra-hackage-repos    | no            | nothing    |
              # extra-hackage-tarballs | no            | workaround |
              # -----------------------+---------------+------------+
              #
              final.runCommand "dot-cabal" {
                nativeBuildInputs = [ nix-tools.exes.cabal final.xorg.lndir ] ++ cabal-issue-8352-workaround;
              } ''
                # prepopulate hackage
                mkdir -p $out/packages/hackage.haskell.org
                lndir ${bootstrapped-hackage-tarball} $out/packages/hackage.haskell.org
                # prepopulate extra-hackage-repos
                ${final.lib.concatStrings (final.lib.mapAttrsToList (name: repo: ''
                  mkdir -p $out/packages/${name}
                  lndir ${repo} $out/packages/${name}
                '') extra-hackage-repos)}
                # prepopulate extra-hackage-tarballs
                ${final.lib.concatStrings (final.lib.mapAttrsToList (name: repo: ''
                  mkdir -p $out/packages/${name}
                  lndir ${repo} $out/packages/${name}
                '') bootstrapped-extra-hackage-tarballs)}
                # Write global cabal config
                cat >$out/config <<EOF
                repository hackage.haskell.org
                  url: http://hackage.haskell.org/
                  secure: True
                ${final.lib.concatStrings (final.lib.mapAttrsToList (name: _repo: ''
                  repository ${name}
                    url: http://${name}/
                    secure: True
                '') bootstrapped-extra-hackage-tarballs)}
                EOF
              '';

        # Some of features of haskell.nix rely on using a hackage index
        # to calculate a build plan.  To maintain stability for caching and
        # to allow the outputs to be materialized we pin this value here.
        # If you want to update this value it important to check the
        # materializations.  Turn `checkMaterialization` on below and
        # check the CI results before turning it off again.
        internalHackageIndexState = "2024-10-17T00:00:00Z"; # Remember to also update ../nix-tools/cabal.project and ../nix-tools/flake.lock

        checkMaterialization = false; # This is the default. Use an overlay to set it to true and test all the materialized files

        # Helps materialize the output of derivations
        materialize = import ../lib/materialize.nix {
          pkgs = final.pkgsBuildBuild;
          inherit (final.pkgsBuildBuild) nix runCommand writeShellScript;
          inherit (final.haskell-nix) checkMaterialization;
        };

        update-index-state-hashes = import ../scripts/update-index-state-hashes.nix {
            inherit (final.haskell-nix) indexStateHashesPath;
            inherit (final) coreutils nix writeShellScriptBin stdenv lib curl;
            # Update scripts use the internal nix-tools (compiled with a fixed GHC version)
            nix-tools = final.haskell-nix.nix-tools-unchecked;
        };

        # Function to call stackToNix
        callStackToNix = import ../lib/call-stack-to-nix.nix;

        # given a source location call `cabal-to-nix` (from nix-tools) on it
        # to produce the nix representation of it.
        callCabalToNix = { name, src, cabal-file ? "${name}.cabal" }:
            final.buildPackages.pkgs.runCommand "${name}.nix" {
                # This function is only used when building stack projects (via mkCacheLine and mkCacheFile)
                # When building stack projects we use the unchecked nix-tools (compiled with a fixed GHC version)
                nativeBuildInputs = [ final.buildPackages.haskell-nix.nix-tools-unchecked ];

                LOCALE_ARCHIVE = final.lib.optionalString (final.stdenv.buildPlatform.libc == "glibc") "${final.buildPackages.glibcLocales}/lib/locale/locale-archive";
                LANG = "en_US.UTF-8";
                LC_ALL = "en_US.UTF-8";
            } ''
            cabal-to-nix "${src}" "${src}/${cabal-file}" > "$out"
            '';


        # Given a single cache entry:
        # { name = ...; url = ...; rev = ...; ref = ...; sha256 = ...; cabal-file = ...; type = ...; is-private = ...; }
        # compute a string that represents this cache entry:
        # "${url} ${rev} ${subdir} ${sha256} ${name} ${nix-expr}"
        #
        # This handles private repositories with the `is-private` argument
        # (with `builtins.fetchGit`), as well as handling stack-based projects
        # with the `type` argument.
        mkCacheLine = { name, url, rev, ref ? null, subdir ? ".", sha256 ? null, cabal-file ? "${name}.cabal", type ? "cabal", is-private ? false }:
          let
            # Fetch the entire repo, using either pkgs.fetchgit or
            # builtins.fetchGit depending on whether the repo is private.
            entireRepo =
              if is-private
              then
                # It doesn't make sense to specify sha256 on a private repo
                # because it is not used by buitins.fetchGit.
                assert isNull sha256;
                builtins.fetchGit
                  ({ inherit url rev; } //
                      final.buildPackages.lib.optionalAttrs (ref != null) { inherit ref; }
                  )
              else
                # Non-private repos must have sha256 set.
                assert sha256 != null;
                # pkgs.fetchgit doesn't have any way of fetching from a given
                # ref.
                assert isNull ref;
                final.fetchgit {
                  url = url;
                  rev = rev;
                  sha256 = sha256;
                };

            # This is basically entireRepo, but focused on the subdir if it is specified.
            repoWithSubdir =
              entireRepo + (if subdir == "." then "" else "/" + subdir);

            nix-expr =
              if type == "cabal"
              then
                final.buildPackages.haskell-nix.callCabalToNix {
                  src = repoWithSubdir;
                  inherit name cabal-file;
                }
              else if type == "stack"
              then
                (final.buildPackages.haskell-nix.callStackToNix {
                  src = repoWithSubdir;
                  inherit name subdir;
                }).projectNix
              else
                throw "Unknown type '${type}` for a cache entry";

            sha256String = if isNull sha256 then final.buildPackages.lib.fakeSha256 else sha256;

          in {
            line = "${url} ${rev} ${subdir} ${sha256String} ${name}";
            inherit nix-expr;
          };

        # Given a list of repos:
        # [ { name = ...; url = ...; rev = ...; ref = ...; sha256 = ...; cabal-file = ...; type = ...; is-private = ...; } ]
        # produce a cache file that can be used for
        # stack-to-nix or plan-to-nix to prevent them
        # from needing network access.
        # The cache contains only local paths to nix files so that it can
        # the results of `stack-to-nix` can be imported in restricted eval
        # mode.
        mkCacheFile = repos:
          final.buildPackages.pkgs.runCommand "cache-file" {} ''
              mkdir -p $out
              touch $out/.stack-to-nix.cache
              ${final.lib.concatStrings (
                final.lib.lists.zipListsWith (n: repo:
                  let l = mkCacheLine repo;
                  in ''
                    cp ${l.nix-expr} $out/.stack-to-nix.cache.${toString n}
                    echo ${l.line} .stack-to-nix.cache.${toString n} >> $out/.stack-to-nix.cache
                  '')
                  (final.lib.lists.range 0 ((builtins.length repos) - 1))
                  repos)
              }
          '';

        genStackCache = import ../lib/stack-cache-generator.nix;

        mkCacheModule = cache:
            # for each item in the `cache`, set
            #   packages.$name.src = fetchgit ...
            # and
            #   packages.$name.postUnpack = ...
            # if subdir is given.
            #
            # We need to do this, as cabal-to-nix will generate
            # src = /nix/store/... paths, and when we build the
            # package we won't have access to the /nix/store
            # path.  As such we regenerate the fetchgit command
            # we used in the first place, and thus have a proper
            # src value.
            #
            # TODO: this should be moved into `call-stack-to-nix`
            { packages =
                let
                  repoToAttr = { name, url, rev, ref ? null, sha256 ? null, subdir ? null, is-private ? false, ... }: {
                    ${name} = {
                      src =
                        if is-private
                        then
                          builtins.fetchGit
                            ({ inherit url rev; } //
                              final.buildPackages.lib.optionalAttrs (ref != null) { inherit ref; }
                            )
                        else
                          final.fetchgit { inherit url rev sha256; };
                    } // final.buildPackages.lib.optionalAttrs (subdir != null && subdir != ".") { postUnpack = "sourceRoot+=/${subdir}; echo source root reset to $sourceRoot"; };
                  };

                  cacheMap = builtins.map repoToAttr cache;
                in
                builtins.foldl' (x: y: x // y) {} cacheMap;

            };

        dummy-ghc-pkg-dump = final.callPackage ../lib/dummy-ghc-pkg-dump {
          # FIXME: just for testing
          ghc-version = final.ghc.version;
          ghc-src = final.ghc.src;
        };
        
        # Takes a haskell src directory runs cabal new-configure and plan-to-nix.
        # Resulting nix files are added to nix-plan subdirectory.
        callCabalProjectToNix = import ../lib/call-cabal-project-to-nix.nix {
            index-state-hashes = import indexStateHashesPath;
            inherit (final.buildPackages.haskell-nix) haskellLib;
            pkgs = final.buildPackages.pkgs;
            inherit (final.buildPackages.pkgs) cacert;
        };

        # Loads a plan and filters the package directories using cleanSourceWith
        importAndFilterProject = import ../lib/import-and-filter-project.nix {
            inherit (final.buildPackages.haskell-nix) haskellLib;
            pkgs = final.buildPackages.pkgs;
        };

        # Loads a plan and filters the package directories using cleanSourceWith
        loadCabalPlan = import ../lib/load-cabal-plan.nix {
            inherit (final.buildPackages.haskell-nix) haskellLib;
            pkgs = final.buildPackages.pkgs;
        };

        # References to the unpacked sources, for caching in a Hydra jobset.
        source-pins = import ../lib/make-source-pins.nix {
            inherit (final) lib writeTextFile;
            sources = [ hackageSrc stackageSrc final.path ];
        };

        # -- IFDs --
        # Build a specific package (name, version) against a given index-stage
        # from hackage.  This is useful if you want to build an executable from
        # a given package.
        # NB: If no explicit index-state is provided the most recent one from
        # the index-state-hashes is used.  This guarantees reproducibility wrt
        # to the haskell.nix revision.  If reproducibility beyond haskell.nix
        # is required, a specific index-state should be provided!
        hackage-package = projectModule:
          let project = hackage-project projectModule;
          in project.getPackage project.args.name;
        hackage-project = projectModule:
          cabalProject' ([
            (import ../modules/hackage-project.nix)
            ] ++ (import ../modules/hackage-quirks.nix)
              ++ (if builtins.isList projectModule then projectModule else [projectModule]));

        # This function is like `cabalProject` but it makes the plan-nix available
        # separately from the hsPkgs.  The advantage is that the you can get the
        # plan-nix without building the project.
        cabalProject' =
          projectModule: haskellLib.evalProjectModule ../modules/cabal-project.nix projectModule (
            { config, options, ... }:
            let
              inherit (config) compiler-nix-name compilerSelection evalPackages;
              selectedCompiler = (compilerSelection final.buildPackages).${compiler-nix-name};
              callProjectResults = callCabalProjectToNix config;
              plan-pkgs = if !builtins.pathExists (callProjectResults.projectNix + "/plan.json")
                then
                  # If there is no `plan.json` file assume this is a materialized
                  # `plan-nix` and use the old code path.
                  # TODO remove this once all the materialized files are updated
                  importAndFilterProject {
                    inherit (callProjectResults) projectNix sourceRepos src;
                  }
                else
                  loadCabalPlan {
                    inherit selectedCompiler callProjectResults;
                  };
              buildProject = if final.stdenv.hostPlatform != final.stdenv.buildPlatform
                then final.pkgsBuildBuild.haskell-nix.cabalProject' projectModule
                else project;
              pkg-set = if plan-pkgs ? configurationError
                then {
                  inherit (plan-pkgs) configurationError;
                  config = {
                    compiler.nix-name = compiler-nix-name;
                    hsPkgs = {};
                    inherit evalPackages;
                  };
                }
                else mkCabalProjectPkgSet
                { inherit compiler-nix-name compilerSelection plan-pkgs;
                  pkg-def-extras = config.pkg-def-extras or [];
                  modules = [ { _module.args.buildModules = final.lib.mkForce buildProject.pkg-set; } ]
                    ++ (config.modules or [])
                    ++ [ {
                      ghc.package =
                        if config.ghcOverride != null
                          then config.ghcOverride
                        else if config.ghc != null
                          then config.ghc
                        else
                          final.lib.mkDefault selectedCompiler;
                      compiler.nix-name = final.lib.mkForce config.compiler-nix-name;
                      evalPackages = final.lib.mkDefault evalPackages;
                    } ];
                  extra-hackages = config.extra-hackages or [] ++ callProjectResults.extra-hackages;
                };

              project = addProjectAndPackageAttrs rec {
                  inherit (pkg-set.config) hsPkgs;
                  inherit pkg-set;
                  inherit options;
                  args = config;
                  plan-nix = callProjectResults.projectNix;
                  inherit (callProjectResults) index-state-max;
                  tool = final.buildPackages.haskell-nix.tool' evalPackages pkg-set.config.compiler.nix-name;
                  tools = final.buildPackages.haskell-nix.tools' evalPackages pkg-set.config.compiler.nix-name;
                  roots = final.haskell-nix.roots pkg-set.config.compiler.nix-name;
                  projectFunction = haskell-nix: haskell-nix.cabalProject';
                  inherit projectModule buildProject;
                };
            in project);

        # Take `hsPkgs` from the `rawProject` and update all the packages and
        # components so they have a `.project` attribute and as well as
        # a `.package` attribute on the components.
        addProjectAndPackageAttrs = let
          # helper function similar to nixpkgs 'makeExtensible' but that keep track
          # of extension function so that it can be reused to extend another project:
          makeExtensible = f: rattrs: final.lib.fix (final.lib.extends f (self: rattrs self // {
            __overlay__ = f;
            extend = f: makeExtensible (final.lib.composeExtensions self.__overlay__ f) rattrs;
            appendOverlays = extraOverlays: self.extend (final.lib.composeManyExtensions ([self.__overlay__] ++ extraOverlays));
          }));
         in rawProject:
          makeExtensible (_final: _prev: {}) (project':
            let project = project' // { recurseForDerivations = false; };
            in rawProject // rec {
              # It is often handy to be able to get nix pkgs from the project.
              pkgs = final;
              # Haskell packages
              hsPkgs = final.lib.mapAttrs (packageName: package':
                if package' == null
                  then null
                  else
                    let package = package' // { recurseForDerivations = false; };
                    in package' // rec {
                      components = final.lib.mapAttrs (n: v:
                        if n == "library" || n == "all"
                          then v // { inherit project package; }
                          else final.lib.mapAttrs (_: c: c // { inherit project package; }) v
                      ) package'.components;
                      inherit project;

                      # Look up a component in the package based on ctype:name
                      getComponent = componentName:
                        let m = builtins.match "(lib|flib|exe|test|bench):([^:]*)" componentName;
                        in
                          assert final.lib.asserts.assertMsg (m != null)
                            "Invalid package component name ${componentName}.  Expected it to start with one of lib: flib: exe: test: or bench:";
                          if builtins.elemAt m 0 == "lib" && builtins.elemAt m 1 == packageName
                            then components.library
                            else components.${haskellLib.prefixComponent.${builtins.elemAt m 0}}.${builtins.elemAt m 1};

                      coverageReport = haskellLib.coverageReport ({
                        name = package.identifier.id;
                        # Include the checks for a single package.
                        checks = final.lib.filter (final.lib.isDerivation) (final.lib.attrValues package'.checks);
                        # Checks from that package may provide coverage information for any library in the project.
                        mixLibraries = final.lib.concatMap
                          (pkg: final.lib.optional (pkg.components ? library) pkg.components.library)
                            (final.lib.attrValues (haskellLib.selectProjectPackages project.hsPkgs));
                        ghc = project.pkg-set.config.ghc.package;
                      });
                    }
                ) (builtins.removeAttrs rawProject.hsPkgs
                  # These are functions not packages
                  [ "shellFor" "makeConfigFiles" "ghcWithHoogle" "ghcWithPackages" "buildPackages" ]);

            projectCoverageReport = haskellLib.projectCoverageReport project (map (pkg: pkg.coverageReport) (final.lib.attrValues (haskellLib.selectProjectPackages hsPkgs)));

            # `projectCross` is like `pkgsCross`, but for haskell.nix projects.
            # To get a cross platform version of the project use
            # `projectCross.<system>` where system is a member of nixpkgs lib.systems.examples.
            # See https://nixos.wiki/wiki/Cross_Compiling
            projectCross = (final.lib.mapAttrs (_: pkgs:
                (rawProject.projectFunction pkgs.haskell-nix rawProject.projectModule)
                # Re-apply overlay from original project:
                .extend project.__overlay__
              ) final.pkgsCross) // { recurseForDerivations = false; };

            # attribute set of variant (with an extra module applied) for the project,
            # mapped from `flake.variants` config values.
            projectVariants = final.lib.mapAttrs (_: project.appendModule) project.args.flake.variants;

            # re-eval this project with an extra module (or module list).
            appendModule = extraProjectModule: (rawProject.projectFunction final.haskell-nix
              ((if builtins.isList rawProject.projectModule
                then rawProject.projectModule
                else [rawProject.projectModule])
              ++ (if builtins.isList extraProjectModule
                then extraProjectModule
                else [extraProjectModule])))
                # Re-apply overlay from original project:
                .extend project.__overlay__;

            # Add support for passing in `crossPlatforms` argument.
            # crossPlatforms is an easy way to include the inputs for a basic
            # cross platform shell in a native shell.
            #
            # For instance if `default.nix` is a project, then `shell.nix` can be:
            #   (import ./. {}).shellFor {
            #     tools.cabal = {};
            #     crossPlatforms = p: [ p.ghcjs ];
            #   }
            #
            # This adds support for compiling with ghcjs.  To build use the cabal wrapper:
            #   js-unknown-ghcjs-cabal build all
            #
            # ## How it Works
            #
            # The cross compilation shells are made using the `projectCross` attribute
            # to get the selected cross compilation projects (e.g. project.projectCross.ghcjs).
            #
            # The `shellFor` function for those projects is called with arguments based on the
            # ones used for the main shell (the `withHoogle` argument is set to `false`).
            #
            # These shells are added to the main shell using the `inputsFrom` argument.
            #
            # Without `crossPlatforms` the above example would be:
            #   let project = import ./. {};
            #   in project.shellFor {
            #     tools.cabal = {};
            #     inputsFrom = [
            #       (project.platformCross.ghcjs.shellFor { withHoogle = false; })
            #     ];
            #   }
            #
            shellFor = shellArgs:
              let
                # These are the args we will pass to the main shell.
                args' = builtins.removeAttrs shellArgs [ "crossPlatforms" ];
                # These are the args we will pass to the shells for the corss compiler
                argsCross =
                  # These things should match main shell
                  final.lib.filterAttrs (n: _: builtins.elem n [
                    "packages" "components" "additional" "exactDeps" "packageSetupDeps"
                  ]) shellArgs // {
                    # The main shell's hoogle will probably be faster to build.
                    withHoogle = false;
                  };
                # These are the cross compilation versions of the project we will include.
                selectedCrossProjects =
                  if shellArgs ? crossPlatforms
                    then shellArgs.crossPlatforms projectCross
                    else [];
                # Shells for cross compilation
                crossShells = builtins.map (project: project.shellFor argsCross)
                  selectedCrossProjects;
              in rawProject.hsPkgs.shellFor (args' // {
                  # Add inputs from the cross compilation shells
                  inputsFrom = args'.inputsFrom or [] ++ crossShells;
                });

            # Default shell
            shell = shellFor rawProject.args.shell;

            # Like `.hsPkgs.${packageName}` but when compined with `getComponent` any
            # cabal configure errors are defered until the components derivation builds.
            getPackage = packageName:
              if rawProject.pkg-set ? configurationError
                then
                  # A minimal proxy for a package when cabal configure failed
                  let package = {
                    # Including the project so that things like:
                    #  (p.getPackage "hello").project.tool "hlint" "latest"
                    # will still work even if "hello" failed to configure.
                    inherit project;

                    # Defer configure time errors for the library component
                    #  (p.getPackage "hello").components.library
                    components.library = package.getComponent "lib:${packageName}";

                    # This procide a derivation (even though the component may
                    # not exist at all).  The derivation will never build
                    # and simple outputs the result of cabal configure.
                    getComponent = componentName:
                      final.rawProject.config.evalPackages.runCommand "cabal-configure-error" {
                        passthru = {
                          inherit project package;
                        };
                      } ''
                        cat ${rawProject.pkg-set.configurationError}
                        echo Unable to find component ${packageName}:${componentName}  \
                          due to the above cabal configuration error
                        exit 1
                      '';
                  };
                  in package
                else project.hsPkgs.${packageName};

            # Look a component in the project based on `pkg:ctype:name`
            getComponent = componentName:
              let m = builtins.match "([^:]*):(lib|flib|exe|test|bench):([^:]*)" componentName;
              in
                assert final.lib.asserts.assertMsg (m != null)
                  "Invalid package component name ${componentName}.  Expected package:ctype:component (where ctype is one of lib, flib, exe, test, or bench)";
                (getPackage (builtins.elemAt m 0)).getComponent "${builtins.elemAt m 1}:${builtins.elemAt m 2}";

            # Helper function that can be used to make a Nix Flake out of a project
            # by including a flake.nix.  See docs/tutorials/getting-started-flakes.md
            # for an example flake.nix file.
            # This flake function maps the build outputs to the flake `packages`,
            # `checks` and `apps` output attributes.
            flake' =
              let
                combinePrefix = a: b: if a == "default" then b else "${a}-${b}";
                mkFlake = project: haskellLib.mkFlake project rec {
                  selectPackages = project.args.flake.packages;
                  coverage = final.lib.optionalAttrs project.args.flake.doCoverage
                    (haskellLib.projectCoverageCiJobs
                      project selectPackages project.args.flake.coverageProjectModule);
                };
                forAllCrossCompilers = prefix: project: (
                    [{ ${prefix} = mkFlake project; }]
                  ++ (map (project: {
                       ${combinePrefix prefix project.pkgs.stdenv.hostPlatform.config} =
                         mkFlake project;
                      })
                     (project.args.flake.crossPlatforms project.projectCross)
                  ));
                forAllVariants =
                    forAllCrossCompilers "default" project
                  ++ final.lib.concatLists (final.lib.mapAttrsToList
                    (name: projectVariant: forAllCrossCompilers name projectVariant)
                     project.projectVariants);
              in haskellLib.combineFlakes ":" (builtins.foldl' (a: b: a // b) {} forAllVariants);
            flake = args: (project.appendModule { flake = args; }).flake';

            inherit (rawProject) args;
            inherit (rawProject.hsPkgs) makeConfigFiles ghcWithHoogle ghcWithPackages;
          });

        cabalProject = args: let p = cabalProject' args;
            in p.hsPkgs // p;

        stackProject' =
          projectModule: haskellLib.evalProjectModule ../modules/stack-project.nix projectModule (
            { config, options, ... }:
            let inherit (config) evalPackages;
                callProjectResults = callStackToNix (config
                  // final.lib.optionalAttrs (config.cache == null) { inherit cache; });
                generatedCache = genStackCache config;
                cache = if config.cache != null then config.cache else generatedCache;
            in let
              buildProject = if final.stdenv.hostPlatform != final.stdenv.buildPlatform
                then final.pkgsBuildBuild.haskell-nix.stackProject' projectModule
                else project;
              pkg-set = mkStackPkgSet
                { stack-pkgs = importAndFilterProject callProjectResults;
                  pkg-def-extras = (config.pkg-def-extras or []);
                  modules = [ { _module.args.buildModules = final.lib.mkForce buildProject.pkg-set; }
                      (mkCacheModule cache) ]
                    ++ (config.modules or [])
                    ++ final.lib.optional (config.ghc != null) { ghc.package = config.ghc; }
                    ++ final.lib.optional (config.compiler-nix-name != null)
                        { compiler.nix-name = final.lib.mkForce config.compiler-nix-name; }
                    ++ [ { evalPackages = final.lib.mkDefault evalPackages; } ];
                };

                project = addProjectAndPackageAttrs {
                  inherit (pkg-set.config) hsPkgs;
                  inherit pkg-set;
                  inherit options;
                  args = config;
                  stack-nix = callProjectResults.projectNix;
                  tool = final.buildPackages.haskell-nix.tool' evalPackages pkg-set.config.compiler.nix-name;
                  tools = final.buildPackages.haskell-nix.tools' evalPackages pkg-set.config.compiler.nix-name;
                  roots = final.haskell-nix.roots pkg-set.config.compiler.nix-name;
                  projectFunction = haskell-nix: haskell-nix.stackProject';
                  inherit projectModule buildProject;
                };
            in project);

        stackProject = args: let p = stackProject' args;
            in p.hsPkgs // p;

        # `project'` and `project` automatically select between `cabalProject`
        # and `stackProject` (when possible) by looking for `stack.yaml` or
        # `cabal.project` files.  If both exist it uses the `cabal.project` file.
        # To override this pass in:
        #     `projectFileName = "stack.yaml;"`
        # If the selected file ends in a `.yaml` it is assumed to be for `stackProject`.
        # If neither `stack.yaml` nor `cabal.project` exist `cabalProject` is
        # used (as it will use a default `cabal.project`).
        project' = projectModule:
          let
            projectModule' = if builtins.isList projectModule then projectModule else [projectModule];
            inherit ((final.lib.evalModules {
              modules = [
                (import ../modules/project-common.nix)
                (import ../modules/stack-project.nix)
                (import ../modules/cabal-project.nix)
                (import ../modules/project.nix)
                {_module.args.pkgs = final;} # Needed to make `src = config.evalPackages.haskell-nix.haskellLib.cleanGit ...` work
              ] ++ projectModule';
            }).config) src projectFileName;
            dir = __readDir (src.origSrcSubDir or src);
            exists = fileName: builtins.elem (dir.${fileName} or "") ["regular" "symlink"];
            stackYamlExists    = exists "stack.yaml";
            cabalProjectExists = exists "cabal.project";
            selectedFileName =
              if projectFileName != null
                then projectFileName  # Prefer the user selected project file name
                else
                  if stackYamlExists && cabalProjectExists
                    then __trace ("haskell-nix.project : both `stack.yaml` and `cabal.project` files exist.  Using `cabal.project`. "
                      + "set `projectFileName = \"stack.yaml\";` to override this.`") "cabal.project"
                    else
                      if stackYamlExists
                        then "stack.yaml"      # stack needs a stack.yaml
                        else "cabal.project";  # the cabal.project file is optional
          in
            if final.lib.hasSuffix ".yaml" selectedFileName
              then stackProject' ([
                    (import ../modules/project.nix)
                    { stackYaml = selectedFileName; }
                  ] ++ projectModule'
                )
              else cabalProject' ([
                    (import ../modules/project.nix)
                    { cabalProjectFileName = selectedFileName; }
                  ] ++ projectModule');

        # This is the same as the `cabalPackage` and `stackPackage` wrappers
        # for `cabalPackage` and `stackPackage`.
        project = args: let p = project' args;
          in p.hsPkgs // p;

        # Like `cabalProject'`, but for building the GHCJS compiler.
        # This is exposed to allow GHCJS developers to work on the GHCJS
        # code in a nix-shell with `shellFor`.
        ghcjsProject = import ../lib/ghcjs-project.nix { pkgs = final; materialized-dir = ../materialized; };

        # The functions that return a plan-nix often have a lot of dependencies
        # that could be GCed and also will not make it into hydra cache.
        # Use this `withInputs` function to make sure your tests include
        # the dependencies needed explicitly.  For example, if you have:
        #   project = cabalProject' {...};
        # In your tests module add something that is effectively
        #   testProjectPlan = withInputs project.plan-nix;
        withInputs = final.recurseIntoAttrs;

        iserv-proxy-exes = __mapAttrs (compiler-nix-name: _ghc:
            let
              exes = pkgs: (pkgs.haskell-nix.cabalProject' ({pkgs, ...}: {
                name = "iserv-proxy";
                inherit compiler-nix-name;

                src = sources.iserv-proxy;

                modules = [{
                  config = {
                    # Prevent the iserve-proxy-interpreter from depending on itself
                    # by disabling the `--ghc-option` normally passed to `setupBuildFlags`
                    # when cross compiling.
                    setupBuildFlags = final.lib.mkForce [];
                  };
                }];
              } // final.lib.optionalAttrs (__compareVersions final.buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.10" > 0) {
                  cabalProjectLocal = ''
                    allow-newer: *:base, *:bytestring
                  '';
                })).hsPkgs.iserv-proxy.components.exes;
            in {
              # We need the proxy for the build system and the interpreter for the target
              inherit (exes final.pkgsBuildBuild) iserv-proxy;
              inherit (exes final) iserv-proxy-interpreter;
            }) final.haskell-nix.compiler;

        # Add this to your tests to make all the dependencies of haskell.nix
        # are tested and cached. Consider using `p.roots` where `p` is a
        # project as it will automatically match the `compiler-nix-name`
        # of the project.
        roots = compiler-nix-name: final.linkFarm "haskell-nix-roots-${compiler-nix-name}"
          (final.lib.filter (x: x.name != "recurseForDerivations")
            (final.lib.mapAttrsToList (name: path: { inherit name path; })
              (roots' compiler-nix-name 2)));

        roots' = compiler-nix-name: ifdLevel:
          	final.recurseIntoAttrs ({
            # Things that require no IFD to build
            source-pin-hackage = hackageSrc;
            source-pin-stackage = stackageSrc;
            source-pin-haskell-nix = final.path;
            # Double buildPackages is intentional,
            # see comment in lib/default.nix for details.
            # Using buildPackages rather than evalPackages so both darwin and linux
            # versions will get pinned (evalPackages on darwin systems will be for darwin).
            inherit (final.buildPackages.buildPackages) gitMinimal nix-prefetch-git;
            inherit (final.buildPackages) nix;
          } // final.lib.optionalAttrs (final.stdenv.hostPlatform.libc == "glibc") {
            inherit (final) glibcLocales;
          } // final.lib.optionalAttrs (ifdLevel > 0) {
            # Things that require one IFD to build (the inputs should be in level 0)
            boot-alex = final.buildPackages.haskell-nix.bootstrap.packages.alex;
            boot-happy = final.buildPackages.haskell-nix.bootstrap.packages.happy;
            boot-hscolour = final.buildPackages.haskell-nix.bootstrap.packages.hscolour;
            ghc = final.buildPackages.haskell-nix.compiler.${compiler-nix-name};
            ghc-boot-packages-nix = final.recurseIntoAttrs
              final.ghc-boot-packages-nix.${compiler-nix-name};
          } // final.lib.optionalAttrs (ifdLevel > 1) {
            # Things that require two levels of IFD to build (inputs should be in level 1)
            nix-tools-unchecked = final.pkgsBuildBuild.haskell-nix.nix-tools-unchecked;
          } // final.lib.optionalAttrs (ifdLevel > 1
            && final.haskell-nix.haskellLib.isCrossHost
            # GHCJS builds its own template haskell runner.
            # These seem to be the only things we use from `ghc-extra-packages`
            # in haskell.nix itself.
            && !final.stdenv.hostPlatform.isGhcjs)
              final.haskell-nix.iserv-proxy-exes.${compiler-nix-name});
    };
}
