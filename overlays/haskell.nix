{ sources,  ... }:
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

        # Additional user-provided mappings to augment ./../lib/pkgconf-nixpkgs-map.nix
        extraPkgconfigMappings = {};
        # Nix Flake based source pins.
        # To update all inputs, get unstable Nix and then `nix flake update --recreate-lock-file`
        # Or `nix-shell -p nixUnstable --run "nix --experimental-features 'nix-command flakes' flake update --recreate-lock-file"`
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
                package.compiler-nix-name.version = final.buildPackages.haskell-nix.compiler.${compiler-nix-name'}.version;
                plan.compiler-nix-name.version = final.buildPackages.haskell-nix.compiler.${(plan-pkgs.pkgs hackage).compiler.nix-name}.version;
                withMsg = final.lib.assertMsg;
            in
              # Check that the GHC version of the selected compiler matches the one of the plan
              assert (withMsg
                (package.compiler-nix-name.version
                  == plan.compiler-nix-name.version)
                ''
                The compiler versions for the package and the plan don't match.
                       Make sure you didn't forget to update plan-sha256.''
              );
              mkPkgSet {
                inherit pkg-def;
                pkg-def-extras = [ plan-pkgs.extras
                                   # Using the -unchecked version here to avoid infinite
                                   # recursion issues when checkMaterialization = true
                                   final.ghc-boot-packages-unchecked.${compiler-nix-name'}
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
            { "hackage.haskell.org-at-${at}" = final.fetchurl {
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

        dotCabal = { index-state, sha256, cabal-install, extra-hackage-tarballs ? {}, extra-hackage-repos ? {}, ... }@args:
            let
              allTarballs = hackageTarball args // extra-hackage-tarballs;
              allNames = final.lib.concatStringsSep "-" (builtins.attrNames allTarballs);
              # Main Hackage index-state is embedded in its name and thus will propagate to
              # dotCabalName anyway.
              dotCabalName = "dot-cabal-" + allNames;
              tarballRepoFor = name: index: final.runCommand "tarballRepo_${name}" {
                nativeBuildInputs = [ cabal-install ] ++ cabal-issue-8352-workaround;
              } ''
                set -xe

                mkdir -p $out/.cabal
                cat <<EOF > $out/.cabal/config
                repository ${name}
                  url: file:${mkLocalHackageRepo { inherit name index; }}
                  secure: True
                  root-keys:
                  key-threshold: 0

                EOF

                # All repositories must be mkdir'ed before calling new-update on any repo,
                # otherwise it fails.
                mkdir -p $out/.cabal/packages/${name}

                HOME=$out cabal new-update ${name}
              '';
              f = name: index:
                let x = tarballRepoFor name index; in
                ''
                  ln -s ${x}/.cabal/packages/${name} $out/.cabal/packages/${name}
                  cat ${x}/.cabal/config >> $out/.cabal/config
                '';
            in
              # Add the extra-hackage-repos where we have all the files needed.
              final.runCommand dotCabalName { nativeBuildInputs = [ final.xorg.lndir ]; } ''
                mkdir -p $out/.cabal/packages
                ${builtins.concatStringsSep "\n" (final.lib.mapAttrsToList f allTarballs)}

                ${final.lib.concatStrings (final.lib.mapAttrsToList (name: repo: ''
                  mkdir -p $out/.cabal/packages/${name}
                  lndir ${repo} $out/.cabal/packages/${name}
                '') extra-hackage-repos)}
            '';

        # Some of features of haskell.nix rely on using a hackage index
        # to calculate a build plan.  To maintain stability for caching and
        # to allow the outputs to be materialized we pin this value here.
        # If you want to update this value it important to check the
        # materializations.  Turn `checkMaterialization` on below and
        # check the CI results before turning it off again.
        internalHackageIndexState = "2022-08-29T00:00:00Z";

        checkMaterialization = false; # This is the default. Use an overlay to set it to true and test all the materialized files

        # Helps materialize the output of derivations
        materialize = import ../lib/materialize.nix {
          pkgs = final;
          inherit (final) nix runCommand writeShellScript;
          inherit (final.haskell-nix) checkMaterialization;
        };

        update-index-state-hashes = import ../scripts/update-index-state-hashes.nix {
            inherit (final.haskell-nix) indexStateHashesPath;
            inherit (final) coreutils nix writeShellScriptBin stdenv lib curl;
            # Update scripts use the internal nix-tools and cabal-install (compiled with a fixed GHC version)
            nix-tools = final.haskell-nix.internal-nix-tools;
        };

        # Function to call stackToNix
        callStackToNix = import ../lib/call-stack-to-nix.nix;

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

        # Takes a haskell src directory runs cabal new-configure and plan-to-nix.
        # Resulting nix files are added to nix-plan subdirectory.
        callCabalProjectToNix = import ../lib/call-cabal-project-to-nix.nix {
            index-state-hashes = import indexStateHashesPath;
            inherit (final.buildPackages.haskell-nix) haskellLib;
            pkgs = final.buildPackages.pkgs;
            inherit (final.buildPackages.pkgs) runCommand cacert;
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
            { src, compiler-nix-name, evalPackages, inputMap, ... }@args:
            let
              callProjectResults = callCabalProjectToNix args;
              plan-pkgs = importAndFilterProject {
                inherit (callProjectResults) projectNix sourceRepos src;
              };
              buildProject = if final.stdenv.hostPlatform != final.stdenv.buildPlatform
                then final.buildPackages.haskell-nix.cabalProject' projectModule
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
                { inherit compiler-nix-name plan-pkgs;
                  pkg-def-extras = args.pkg-def-extras or [];
                  modules = [ { _module.args.buildModules = final.lib.mkForce buildProject.pkg-set; } ]
                    ++ (args.modules or [])
                    ++ final.lib.optional (args.ghcOverride != null || args.ghc != null)
                        { ghc.package = if args.ghcOverride != null then args.ghcOverride else args.ghc; }
                    ++ [ {
                      compiler.nix-name = final.lib.mkForce args.compiler-nix-name;
                      evalPackages = final.lib.mkDefault evalPackages;
                      inputMap = final.lib.mkDefault inputMap;
                    } ];
                  extra-hackages = args.extra-hackages or [] ++ callProjectResults.extra-hackages;
                };

              project = addProjectAndPackageAttrs rec {
                  inherit (pkg-set.config) hsPkgs;
                  inherit pkg-set;
                  plan-nix = callProjectResults.projectNix;
                  inherit (callProjectResults) index-state;
                  tool = final.buildPackages.haskell-nix.tool' evalPackages pkg-set.config.compiler.nix-name;
                  tools = final.buildPackages.haskell-nix.tools' evalPackages pkg-set.config.compiler.nix-name;
                  roots = final.haskell-nix.roots pkg-set.config.compiler.nix-name;
                  projectFunction = haskell-nix: haskell-nix.cabalProject';
                  inherit projectModule buildProject args;
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
          makeExtensible (final: prev: {}) (project':
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

                      coverageReport = haskellLib.coverageReport (rec {
                        name = package.identifier.name + "-" + package.identifier.version;
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
            { src, evalPackages, ... }@args:
            let callProjectResults = callStackToNix (args
                  // final.lib.optionalAttrs (args.cache == null) { inherit cache; });
                generatedCache = genStackCache args;
                cache = if args.cache != null then args.cache else generatedCache;
            in let
              buildProject = if final.stdenv.hostPlatform != final.stdenv.buildPlatform
                then final.buildPackages.haskell-nix.stackProject' projectModule
                else project;
              pkg-set = mkStackPkgSet
                { stack-pkgs = importAndFilterProject callProjectResults;
                  pkg-def-extras = (args.pkg-def-extras or []);
                  modules = [ { _module.args.buildModules = final.lib.mkForce buildProject.pkg-set; }
                      (mkCacheModule cache) ]
                    ++ (args.modules or [])
                    ++ final.lib.optional (args.ghc != null) { ghc.package = args.ghc; }
                    ++ final.lib.optional (args.compiler-nix-name != null)
                        { compiler.nix-name = final.lib.mkForce args.compiler-nix-name; }
                    ++ [ { evalPackages = final.lib.mkDefault evalPackages; } ];
                };

                project = addProjectAndPackageAttrs {
                  inherit (pkg-set.config) hsPkgs;
                  inherit pkg-set;
                  stack-nix = callProjectResults.projectNix;
                  tool = final.buildPackages.haskell-nix.tool' evalPackages pkg-set.config.compiler.nix-name;
                  tools = final.buildPackages.haskell-nix.tools' evalPackages pkg-set.config.compiler.nix-name;
                  roots = final.haskell-nix.roots pkg-set.config.compiler.nix-name;
                  projectFunction = haskell-nix: haskell-nix.stackProject';
                  inherit projectModule buildProject args;
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
          (final.lib.filter (x: x.name != "recurseForDerivations")
            (final.lib.mapAttrsToList (name: path: { inherit name path; })
              (roots' compiler-nix-name 2)));

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
            # The internal versions of nix-tools and cabal-install are occasionally used,
            # but definitely need to be cached in case they are used.
            nix-tools = final.buildPackages.haskell-nix.nix-tools.${compiler-nix-name};
            internal-nix-tools = final.buildPackages.haskell-nix.internal-nix-tools;
            cabal-install = final.buildPackages.haskell-nix.cabal-install.${compiler-nix-name};
            internal-cabal-install = final.buildPackages.haskell-nix.internal-cabal-install;
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
