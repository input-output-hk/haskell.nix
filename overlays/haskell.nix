{ sourcesOverride ? {}
,  ... }:
# The haskell.nix infrastructure
#
# for hygienic reasons we'll use haskell-nix as a prefix.
# Using haskell.nix in nix is awkward as I needs to be quoted.
final: prev: {
    haskell-nix = with final.haskell-nix; {

        # Default modules, these will always be included.
        # They are here to be overridden/added to by other
        # overlays.
        defaultModules = [];

        # Niv based source pins.  See https://github.com/nmattia/niv#niv
        # for details on how to update this using the niv tool
        # or edit nix/sources.json manually if you prefer.
        sources = {
          # Hackage and stackage still updated by scripts
          # that predate our use of niv.  We have moved them
          # here though so that you can still use the
          # sourcesOverride arg or `niv add` to replace them.
          hackage = fetchExternal {
            name     = "hackage-exprs-source";
            specJSON = hackageSourceJSON;
            override = "hackage";
          };
          stackage = fetchExternal {
            name     = "stackage-snapshot-source";
            specJSON = stackageSourceJSON;
            override = "stackage";
          };
        } // (import ../nix/sources.nix) // sourcesOverride;

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
              hackageAll = builtins.foldl' (base: extra: base // extra) hackage extra-hackages;
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
            }@args:
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
                pkg-def = excludeBootPackages null pkg-def;
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
            }:

            let
                compiler-nix-name' =
                  if compiler-nix-name != null
                    then compiler-nix-name
                    else ((plan-pkgs.extras hackage).compiler or (plan-pkgs.pkgs hackage).compiler).nix-name;
                pkg-def = excludeBootPackages compiler-nix-name plan-pkgs.pkgs;
                patchesModule = ghcHackagePatches.${compiler-nix-name'} or {};
            in
              # Check that the GHC version of the selected compiler matches the one of the plan
              assert (final.buildPackages.haskell-nix.compiler.${compiler-nix-name'}.version
                   == final.buildPackages.haskell-nix.compiler.${(plan-pkgs.pkgs hackage).compiler.nix-name}.version);
              mkPkgSet {
                inherit pkg-def;
                pkg-def-extras = [ plan-pkgs.extras
                                   final.ghc-boot-packages.${compiler-nix-name'}
                                 ]
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
            if final.targetPlatform.isAarch64 && final.buildPlatform.isAarch64
            then snapshots."lts-15.13"
            else snapshots."lts-14.13";

        # Produce a fixed output derivation from a moving target (hackage index tarball)
        # Takes desired index-state and sha256 and produces a set { name, index }, where
        # index points to "01-index.tar.gz" file downloaded from hackage.haskell.org.
        hackageTarball = { index-state, sha256, nix-tools ? final.haskell-nix.nix-tools, ... }:
            assert sha256 != null;
            let at = builtins.replaceStrings [":"] [""] index-state; in
            { name = "hackage.haskell.org-at-${at}";
              index = final.evalPackages.fetchurl {
                name = "01-index.tar.gz-at-${at}";
                url = "https://hackage.haskell.org/01-index.tar.gz";
                downloadToTemp = true;
                postFetch = "${nix-tools}/bin/truncate-index -o $out -i $downloadedFile -s ${index-state}";

                outputHashAlgo = "sha256";
                outputHash = sha256;
              };
            };

        # Creates Cabal local repository from { name, index } set.
        mkLocalHackageRepo = import ../mk-local-hackage-repo final;

        dotCabal = { index-state, sha256, cabal-install, extra-hackage-tarballs ? [], ... }@args:
            let
              allTarballs = [ (hackageTarball args) ] ++ extra-hackage-tarballs;
              allNames = final.lib.concatMapStringsSep "-" (tarball: tarball.name) allTarballs;
              # Main Hackage index-state is embedded in its name and thus will propagate to
              # dotCabalName anyway.
              dotCabalName = "dot-cabal-" + allNames;
            in
            # This is very big, and cheap to build: use runCommandLocal to prefer building it locally
            final.evalPackages.runCommandLocal dotCabalName { nativeBuildInputs = [ cabal-install ]; } ''
                mkdir -p $out/.cabal
                cat <<EOF > $out/.cabal/config
                ${final.lib.concatStrings (
                  map (tarball:
                ''
                repository ${tarball.name}
                  url: file:${mkLocalHackageRepo tarball}
                  secure: True
                  root-keys:
                  key-threshold: 0

                '') allTarballs
                )}
                EOF

                # All repositories must be mkdir'ed before calling new-update on any repo,
                # otherwise it fails.
                ${final.lib.concatStrings (map ({ name, ... }: ''
                  mkdir -p $out/.cabal/packages/${name}
                '') allTarballs)}

                ${final.lib.concatStrings (map ({ name, ... }: ''
                  HOME=$out cabal new-update ${name}
                '') allTarballs)}
            '';

        # Some of features of haskell.nix rely on using a hackage index
        # to calculate a build plan.  To maintain stability for caching and
        # to allow the outputs to be materialized we pin this value here.
        # If you want to update this value it important to check the
        # materializations.  Turn `checkMaterialization` on below and
        # check the CI results before turning it off again.
        internalHackageIndexState = "2020-05-31T00:00:00Z";

        checkMaterialization = false; # This is the default. Use an overlay to set it to true and test all the materialized files

        # Helps materialize the output of derivations
        materialize = import ../lib/materialize.nix {
          inherit (final.evalPackages) nix;
          inherit (final.haskell-nix) checkMaterialization;
          pkgs = final.evalPackages.pkgs;
          inherit (final.evalPackages.pkgs) runCommand writeShellScript;
        };

        update-index-state-hashes = import ../scripts/update-index-state-hashes.nix {
            inherit (final.haskell-nix) indexStateHashesPath;
            inherit (final) coreutils nix writeShellScriptBin stdenv curl;
            # Update scripts use the internal nix-tools and cabal-install (compiled with a fixed GHC version)
            nix-tools = final.haskell-nix.internal-nix-tools;
        };

        # Function to call stackToNix
        callStackToNix = import ../lib/call-stack-to-nix.nix {
            pkgs = final.evalPackages.pkgs;
            inherit (final.evalPackages.pkgs) runCommand;
            inherit (final.evalPackages.haskell-nix) mkCacheFile materialize haskellLib;
        };

        # given a source location call `cabal-to-nix` (from nix-tools) on it
        # to produce the nix representation of it.
        callCabalToNix = { name, src, cabal-file ? "${name}.cabal" }:
            final.buildPackages.pkgs.runCommand "${name}.nix" {
                # This function is only used when building stack projects (via mkCacheLine and mkCacheFile)
                # When building stack projects we use the internal nix-tools and cabal-install (compiled with a fixed GHC version)
                nativeBuildInputs = [ final.buildPackages.haskell-nix.internal-nix-tools ];

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
                final.evalPackages.fetchgit {
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

        genStackCache = import ../lib/stack-cache-generator.nix {
            inherit (final.evalPackages) pkgs;
            inherit (final.evalPackages.haskell-nix) haskellLib;
        };

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
                          final.evalPackages.fetchgit { inherit url rev sha256; };
                    } // final.buildPackages.lib.optionalAttrs (subdir != null) { postUnpack = "sourceRoot+=/${subdir}; echo source root reset to $sourceRoot"; };
                  };

                  cacheMap = builtins.map repoToAttr cache;
                in
                builtins.foldl' (x: y: x // y) {} cacheMap;

            };

        # Takes a haskell src directory runs cabal new-configure and plan-to-nix.
        # Resulting nix files are added to nix-plan subdirectory.
        callCabalProjectToNix = import ../lib/call-cabal-project-to-nix.nix {
            index-state-hashes = import indexStateHashesPath;
            inherit (final.buildPackages.haskell-nix) haskellLib materialize;
            pkgs = final.buildPackages.pkgs;
            inherit (final) evalPackages;
            inherit (final.evalPackages.haskell-nix) dotCabal;
            inherit (final.buildPackages.pkgs) runCommand symlinkJoin cacert;
        };

        # Loads a plan and filters the package directories using cleanSourceWith
        importAndFilterProject = import ../lib/import-and-filter-project.nix {
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
        hackage-package =
          { name, compiler-nix-name, ... }@args':
          let args = { caller = "hackage-package"; } // args';
          in (hackage-project args).hsPkgs.${name};
        hackage-project =
            { name
            , compiler-nix-name
            , version ? "latest"
            , revision ? "default"
            , ... }@args':
            let version' = if version == "latest"
                  then builtins.head (
                    builtins.sort
                      (a: b: builtins.compareVersions a b > 0)
                      (builtins.attrNames hackage.${name}))
                  else version;
                args = { caller = "hackage-project"; } // args';
                tarball = final.pkgs.fetchurl {
                  url = "mirror://hackage/${name}-${version'}.tar.gz";
                  inherit (hackage.${name}.${version'}) sha256; };
                rev = hackage.${name}.${version'}.revisions.${revision};
                cabalFile = final.pkgs.fetchurl {
                  url = "https://hackage.haskell.org/package/${name}-${version'}/revision/${toString rev.revNum}.cabal";
                  inherit (rev) sha256;
                };
                revSuffix = final.lib.optionalString (rev.revNum > 0) "-r${toString rev.revNum}";
            in let src = final.buildPackages.pkgs.runCommand "${name}-${version'}${revSuffix}-src" { } (''
                  tmp=$(mktemp -d)
                  cd $tmp
                  tar xzf ${tarball}
                  mv "${name}-${version'}" $out
                '' + final.lib.optionalString (rev.revNum > 0) ''
                  cp ${cabalFile} $out/${name}.cabal
                '');
          in cabalProject' (
            (final.haskell-nix.hackageQuirks { inherit name; version = version'; }) //
              builtins.removeAttrs args [ "version" "revision" ] // { inherit src; });

        # This function is like `cabalProject` but it makes the plan-nix available
        # separately from the hsPkgs.  The advantage is that the you can get the
        # plan-nix without building the project.
        cabalProject' =
            { src, compiler-nix-name, ... }@args':
            let
              args = { caller = "cabalProject'"; } // args';
              callProjectResults = callCabalProjectToNix args;
            in let pkg-set = mkCabalProjectPkgSet
                { inherit compiler-nix-name;
                  plan-pkgs = importAndFilterProject {
                    inherit (callProjectResults) projectNix sourceRepos src;
                  };
                  pkg-def-extras = args.pkg-def-extras or [];
                  modules = (args.modules or [])
                          ++ final.lib.optional (args ? ghcOverride || args ? ghc)
                              { ghc.package = args.ghcOverride or args.ghc; }
                          ++ final.lib.optional (args ? compiler-nix-name)
                              { compiler.nix-name = final.lib.mkForce args.compiler-nix-name; };
                  extra-hackages = args.extra-hackages or [];
                };

                project = addProjectAndPackageAttrs rec {
                  inherit (pkg-set.config) hsPkgs;
                  inherit pkg-set;
                  plan-nix = callProjectResults.projectNix;
                  inherit (callProjectResults) index-state;
                  tool = final.buildPackages.haskell-nix.tool pkg-set.config.compiler.nix-name;
                  tools = final.buildPackages.haskell-nix.tools pkg-set.config.compiler.nix-name;
                  roots = final.haskell-nix.roots pkg-set.config.compiler.nix-name;
                  projectFunction = haskell-nix: haskell-nix.cabalProject';
                  projectArgs = args';
                };
            in project;


        # Take `hsPkgs` from the `rawProject` and update all the packages and
        # components so they have a `.project` attribute and as well as
        # a `.package` attribute on the components.
        addProjectAndPackageAttrs = rawProject:
          final.lib.fix (project':
            let project = project' // { recurseForDerivations = false; };
            in rawProject // rec {
              hsPkgs = final.lib.mapAttrs (n: package':
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

                      coverageReport = haskellLib.coverageReport (rec {
                        name = package.identifier.name + "-" + package.identifier.version;
                        library = if components ? library then components.library else null;
                        checks = final.lib.filter (final.lib.isDerivation) (final.lib.attrValues package'.checks);
                        mixLibraries = final.lib.concatMap
                          (pkg: final.lib.optional (pkg.components ? library) pkg.components.library)
                            (final.lib.attrValues (haskellLib.selectProjectPackages project.hsPkgs));
                        ghc = project.pkg-set.config.ghc.package;
                      });
                    }
                ) (builtins.removeAttrs rawProject.hsPkgs
                  # These are functions not packages
                  [ "shellFor" "makeConfigFiles" "ghcWithHoogle" "ghcWithPackages" ]);

            projectCoverageReport = haskellLib.projectCoverageReport project (map (pkg: pkg.coverageReport) (final.lib.attrValues (haskellLib.selectProjectPackages hsPkgs)));

            projectCross = (final.lib.mapAttrs (_: pkgs:
                rawProject.projectFunction pkgs.haskell-nix rawProject.projectArgs
              ) final.pkgsCross) // { recurseForDerivations = false; };

            inherit (rawProject.hsPkgs) makeConfigFiles ghcWithHoogle ghcWithPackages shellFor;
          });

        cabalProject =
            { src, compiler-nix-name, ... }@args':
            let
              args = { caller = "hackage-package"; } // args';
              p = cabalProject' args;
            in p.hsPkgs // p;

        stackProject' =
            { src, ... }@args:
            let callProjectResults = callStackToNix ({ inherit cache; } // args);
                generatedCache = genStackCache args;
                cache = args.cache or generatedCache;
            in let pkg-set = mkStackPkgSet
                { stack-pkgs = importAndFilterProject callProjectResults;
                  pkg-def-extras = (args.pkg-def-extras or []);
                  modules = final.lib.singleton (mkCacheModule cache)
                    ++ (args.modules or [])
                    ++ final.lib.optional (args ? ghc) { ghc.package = args.ghc; }
                    ++ final.lib.optional (args ? compiler-nix-name)
                        { compiler.nix-name = final.lib.mkForce args.compiler-nix-name; };
                };

                project = addProjectAndPackageAttrs {
                  inherit (pkg-set.config) hsPkgs;
                  inherit pkg-set;
                  stack-nix = callProjectResults.projectNix;
                  tool = final.buildPackages.haskell-nix.tool pkg-set.config.compiler.nix-name;
                  tools = final.buildPackages.haskell-nix.tools pkg-set.config.compiler.nix-name;
                  roots = final.haskell-nix.roots pkg-set.config.compiler.nix-name;
                  projectFunction = haskell-nix: haskell-nix.stackProject';
                  projectArgs = args;
                };
            in project;

        stackProject = args: let p = stackProject' args;
            in p.hsPkgs // p;

        # `project'` and `project` automatically select between `cabalProject`
        # and `stackProject` (when possible) by looking for `stack.yaml` or
        # `cabal.project` files.  If both exist we can pass in one of:
        #     `projectFileName = "stack.yaml;"`
        #     `projectFileName = "cabal.project";`
        # to let it know which to choose (or pick another name).  If the
        # selected file ends in a `.yaml` it is assumed to be for `stackProject`.
        # If neither `stack.yaml` nor `cabal.project` exist `cabalProject` is
        # used (as it will use a default `cabal.project`).
        project' = { src, projectFileName ? null, ... }@args':
          let
            args = { caller = "project'"; } // args';
            dir = __readDir (src.origSrcSubDir or src);
            exists = fileName: builtins.elem (dir.${fileName} or "") ["regular" "symlink"];
            stackYamlExists    = exists "stack.yaml";
            cabalProjectExists = exists "cabal.project";
            selectedFileName =
              if projectFileName != null
                then projectFileName  # Prefer the user selected project file name
                else
                  if stackYamlExists && cabalProjectExists
                    then throw ("haskell-nix.project : both `stack.yaml` and `cabal.project` files exist "
                      + "set `projectFileName = \"stack.yaml\";` or `projectFileName = \"cabal.project\";`")
                    else
                      if stackYamlExists
                        then "stack.yaml"      # stack needs a stack.yaml
                        else "cabal.project";  # the cabal.project file is optional
          in
            if final.lib.hasSuffix ".yaml" selectedFileName
              then stackProject' (args // { stackYaml            = selectedFileName; })
              else cabalProject' (args // { cabalProjectFileName = selectedFileName; });

        # This is the same as the `cabalPackage` and `stackPackage` wrappers
        # for `cabalPackage` and `stackPackage`.
        project = args':
          let
            args = { caller = "project"; } // args';
            p = project' args;
          in p.hsPkgs // p;

        # Like `cabalProject'`, but for building the GHCJS compiler.
        # This is exposed to allow GHCJS developers to work on the GHCJS
        # code in a nix-shell with `shellFor`.
        ghcjsProject = import ../lib/ghcjs-project.nix { pkgs = final; };

        # The functions that return a plan-nix often have a lot of dependencies
        # that could be GCed and also will not make it into hydra cache.
        # Use this `withInputs` function to make sure your tests include
        # the dependencies needed explicitly.  For example, if you have:
        #   project = cabalProject' {...};
        # In your tests module add something that is effectively
        #   testProjectPlan = withInputs project.plan-nix;
        withInputs = final.recurseIntoAttrs;

        # Add this to your tests to make all the dependencies of haskell.nix
        # are tested and cached. Consider using `p.roots` where `p` is a
        # project as it will automatically match the `compiler-nix-name`
        # of the project.
        roots = compiler-nix-name: final.linkFarm "haskell-nix-roots-${compiler-nix-name}"
          (final.lib.mapAttrsToList (name: path: { inherit name path; })
            (roots' compiler-nix-name 2));

        roots' = compiler-nix-name: ifdLevel:
          	final.recurseIntoAttrs ({
            # Things that require no IFD to build
            inherit (final.buildPackages.haskell-nix) source-pins;
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
            ghc-extra-projects-nix =
              final.ghc-extra-projects.${compiler-nix-name}.plan-nix;
          } // final.lib.optionalAttrs (ifdLevel > 1) {
            # Things that require two levels of IFD to build (inputs should be in level 1)
            nix-tools = final.buildPackages.haskell-nix.nix-tools.${compiler-nix-name};
            cabal-install = final.buildPackages.haskell-nix.cabal-install.${compiler-nix-name};
          } // final.lib.optionalAttrs (ifdLevel > 1 && !final.stdenv.hostPlatform.isGhcjs) {
            # GHCJS builds its own template haskell runner.
            # These seem to be the only things we use from `ghc-extra-packages`
            # in haskell.nix itself.
            inherit (final.ghc-extra-packages."${compiler-nix-name}"
              .iserv-proxy.components.exes) iserv-proxy;
            inherit (final.ghc-extra-packages."${compiler-nix-name}"
              .remote-iserv.components.exes) remote-iserv;
          });
    };
}
