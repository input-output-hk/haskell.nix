# The haskell.nix infrastructure
#
# for hygenic reasons we'll use haskell-nix as a prefix.
# Using haskell.nix in nix is awkward as I needs to be quoted.
self: super: {
    haskell-nix = with self.haskell-nix; {

        # Default modules, these will always be included.
        # They are here to be overridden/added to by other
        # overlays.
        defaultModules = [];

        # We provide a `callPackage` function to consumers for
        # convenience.  We will however refrain from using it
        # here and be explicit about imports and dependencies.
        callPackage = super.lib.callPackageWith (self // self.haskell-nix);

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
        inherit (import ../lib/clean-source-haskell.nix { inherit (self) lib; })
          haskellSourceFilter
          cleanSourceHaskell;

        # All packages from Hackage as Nix expressions
        hackageSrc = fetchExternal {
            name     = "hackage-exprs-source";
            specJSON = hackageSourceJSON;
            override = "hackage";
        };
        hackage = import hackageSrc;

        # Contains the hashes of the cabal 01-index.tar.gz for given
        # index states.  Starting from April 1st 2019.
        indexStateHashesPath = hackageSrc + "/index-state-hashes.nix";

        # The set of all Stackage snapshots
        stackageSrc = fetchExternal {
            name     = "stackage-snapshot-source";
            specJSON = stackageSourceJSON;
            override = "stackage";
        };

        stackage = import stackageSrc;

        # Utility functions for working with the component builder.
        haskellLib = let hl = import ../lib {
            inherit (self) stdenv lib runCommand srcOnly;
            inherit (self.buildPackages) git;
            haskellLib = hl;
        }; in hl;

        # Create a Haskell package set based on a cabal build plan (plan-to-nix)
        # and Nix expressions representing cabal packages (cabal-to-nix).
        mkPkgSet =
            { pkg-def  # Base package set. Either from stackage (via stack-to-nix) or from a cabal projects plan file (via plan-to-nix)
            , pkg-def-extras ? [] # Additional packages to augment the Base package set `pkg-def` with.
            , modules ? []
            }@args:

            import ../package-set.nix (args // {
                modules = defaultModules ++ modules;
                pkgs = self;
                inherit hackage pkg-def;
            });

        # Some boot packages (libiserv) are in lts, but not in hackage,
        # so we should not try to get it from hackage based on the stackage
        # info.  Instead we can add ghc-boot-packages to `pkg-def-extras`.
        excludeBootPackages = pkg-def: hackage:
          let original = pkg-def hackage;
              bootPkgNames = self.lib.attrNames
                self.ghc-boot-packages.${(pkg-def hackage).compiler.nix-name};
          in original // {
            packages = self.lib.filterAttrs (n: _: self.lib.all (b: n != b) bootPkgNames)
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
                pkg-def = excludeBootPackages pkg-def;
                pkg-def-extras = [ stack-pkgs.extras
                                   self.ghc-boot-packages.${compiler.nix-name}
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
            }@args:

            let
                pkg-def = plan-pkgs.pkgs;
                # The compiler referenced in the stack config
                compiler = (plan-pkgs.extras hackage).compiler or (pkg-def hackage).compiler;
                patchesModule = ghcHackagePatches.${compiler.nix-name} or {};
            in mkPkgSet {
                inherit pkg-def;
                pkg-def-extras = [ plan-pkgs.extras
                                   self.ghc-boot-packages.${compiler.nix-name}
                                 ]
                             ++ pkg-def-extras;
                # set doExactConfig = true, as we trust cabals resolution for
                # the plan.
                modules = [ { doExactConfig = true; } patchesModule ]
                       ++ modules
                       ++ plan-pkgs.modules or [];
            };

        # Package sets for all stackage snapshots.
        snapshots = import ../snapshots.nix { inherit (self) lib ghc-boot-packages; inherit mkPkgSet stackage excludeBootPackages; };
        # Pick a recent LTS snapshot to be our "default" package set.
        haskellPackages = snapshots."lts-14.13";

        # Programs for generating Nix expressions from Cabal and Stack
        # files. This version of nix-tools may be cross compiled.
        # We probably never want to actually cross compile nix-tools on
        # it's own.
        nix-tools-cross-compiled = self.lib.makeOverridable (import ../nix-tools) {
            inherit (self) pkgs lib symlinkJoin makeWrapper
                           git nix nix-prefetch-git;
            inherit (self.haskell-nix) fetchExternal cleanSourceHaskell mkCabalProjectPkgSet;
            hpack = null; # nix-tools does not use hpack project files
        };
        # While `nix-tools-cross-compiled` may be cross compiled,
        # getting it from `buildPackages` we should get
        # nix-tools suitable for running on the build system.
        nix-tools = self.buildPackages.haskell-nix.nix-tools-cross-compiled;
        # TODO perhaps there is a cleaner way to get a suitable nix-tools.

            # Produce a fixed output derivation from a moving target (hackage index tarball)
        hackageTarball = { index-state, sha256, nix-tools ? self.haskell-nix.nix-tools, ... }:
            assert sha256 != null;
            self.fetchurl {
                name = "01-index.tar.gz-at-${builtins.replaceStrings [":"] [""] index-state}";
                url = "https://hackage.haskell.org/01-index.tar.gz";
                downloadToTemp = true;
                postFetch = "${nix-tools}/bin/truncate-index -o $out -i $downloadedFile -s ${index-state}";

                outputHashAlgo = "sha256";
                outputHash = sha256;
            };

        mkLocalHackageRepo = import ../mk-local-hackage-repo { inherit hackageTarball; pkgs = self; };

        dotCabal = { index-state, sha256, cabal-install, ... }@args:
            self.runCommand "dot-cabal-at-${builtins.replaceStrings [":"] [""] index-state}" { nativeBuildInputs = [ cabal-install ]; } ''
                mkdir -p $out/.cabal
                cat <<EOF > $out/.cabal/config
                repository cached
                    url: file:${mkLocalHackageRepo args}
                    secure: True
                    root-keys:
                    key-threshold: 0
                EOF
                mkdir -p $out/.cabal/packages/cached
                HOME=$out cabal new-update cached
            '';

        checkMaterialization = false; # This is the default. Use an overlay to set it to true and test all the materialized files

        # Helps materialize the output of derivations
        materialize = import ../lib/materialize.nix {
          inherit (self) nix;
          inherit (self.haskell-nix) checkMaterialization;
          pkgs = self.buildPackages.pkgs;
          inherit (self.buildPackages.pkgs) runCommand;
        };

        update-index-state-hashes = import ../scripts/update-index-state-hashes.nix {
            inherit (self.haskell-nix) indexStateHashesPath nix-tools;
            inherit (self) coreutils nix writeShellScriptBin stdenv;
        };

        # Function to call stackToNix
        callStackToNix = import ../lib/call-stack-to-nix.nix {
            pkgs = self.buildPackages.pkgs;
            inherit (self.buildPackages.pkgs) runCommand;
            inherit (self.buildPackages.haskell-nix) nix-tools mkCacheFile materialize;
        };

        # given a source location call `cabal-to-nix` (from nix-tools) on it
        # to produce the nix representation of it.
        callCabalToNix = { name, src, cabal-file ? "${name}.cabal" }:
            self.buildPackages.pkgs.runCommand "${name}.nix" {
                nativeBuildInputs = [ self.buildPackages.haskell-nix.nix-tools ];

                LOCALE_ARCHIVE = self.lib.optionalString (self.stdenv.buildPlatform.libc == "glibc") "${self.buildPackages.glibcLocales}/lib/locale/locale-archive";
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
                      self.buildPackages.lib.optionalAttrs (ref != null) { inherit ref; }
                  )
              else
                # Non-private repos must have sha256 set.
                assert sha256 != null;
                # pkgs.fetchgit doesn't have any way of fetching from a given
                # ref.
                assert isNull ref;
                self.buildPackages.pkgs.fetchgit {
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
                self.buildPackages.haskell-nix.callCabalToNix {
                  src = repoWithSubdir;
                  inherit name cabal-file;
                }
              else if type == "stack"
              then
                (self.buildPackages.haskell-nix.callStackToNix {
                  src = repoWithSubdir;
                  inherit name subdir;
                }).projectNix
              else
                throw "Unknown type '${type}` for a cache entry";

            sha256String = if isNull sha256 then self.buildPackages.lib.fakeSha256 else sha256;

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
        # the results of `stack-to-nix` can be imported in restrected eval
        # mode.
        mkCacheFile = repos:
          self.buildPackages.pkgs.runCommand "cache-file" {} ''
              mkdir -p $out
              touch $out/.stack-to-nix.cache
              ${self.lib.concatStrings (
                self.lib.lists.zipListsWith (n: repo:
                  let l = mkCacheLine repo;
                  in ''
                    cp ${l.nix-expr} $out/.stack-to-nix.cache.${toString n}
                    echo ${l.line} .stack-to-nix.cache.${toString n} >> $out/.stack-to-nix.cache
                  '')
                  (self.lib.lists.range 0 ((builtins.length repos) - 1))
                  repos)
              }
          '';

        genStackCache = import ../lib/stack-cache-generator.nix {
            inherit (self.buildPackages) pkgs;
            inherit (self.buildPackages.haskell-nix) haskellLib nix-tools;
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
                              self.buildPackages.lib.optionalAttrs (ref != null) { inherit ref; }
                            )
                        else
                          self.buildPackages.pkgs.fetchgit { inherit url rev sha256; };
                    } // self.buildPackages.lib.optionalAttrs (subdir != null) { postUnpack = "sourceRoot+=/${subdir}; echo source root reset to $sourceRoot"; };
                  };

                  cacheMap = builtins.map repoToAttr cache;
                in
                builtins.foldl' (x: y: x // y) {} cacheMap;

            };

        # Takes a haskell src directory runs cabal new-configure and plan-to-nix.
        # Resulting nix files are added to nix-plan subdirectory.
        callCabalProjectToNix = import ../lib/call-cabal-project-to-nix.nix {
            index-state-hashes = import indexStateHashesPath;
            inherit (self.buildPackages.haskell-nix) dotCabal nix-tools haskellLib materialize;
            pkgs = self.buildPackages.pkgs;
            inherit (self.buildPackages.haskell-nix.haskellPackages.hpack.components.exes) hpack;
            inherit (self.buildPackages.haskell-nix) cabal-install ghc;
            inherit (self.buildPackages.pkgs) runCommand symlinkJoin cacert;
        };

        # Loads a plan and filters the package directories using cleanSourceWith
        importAndFilterProject = import ../lib/import-and-filter-project.nix {
            inherit (self.buildPackages.haskell-nix) haskellLib;
            pkgs = self.buildPackages.pkgs;
        };

        # References to the unpacked sources, for caching in a Hydra jobset.
        source-pins = import ../lib/make-source-pins.nix {
            inherit (self) lib writeTextFile;
            sources = [ hackageSrc stackageSrc self.path ];
        };

        # -- IFDs --
        # Build a specific package (name, version) against a given index-stage
        # from hackage.  This is useful if you want to build an executable from
        # a given package.
        # NB: If no explicit index-state is provided the most recent one from
        # the index-state-hashes is used.  This guarantees reproducability wrt
        # to the haskell.nix revision.  If reproducability beyond haskell.nix
        # is required, a specific index-state should be provided!
        hackage-package = { name, ... }@args: (hackage-project args).hsPkgs.${name};
        hackage-project =
            { name
            , version
            , ... }@args:
            let tarball = self.pkgs.fetchurl {
                url = "mirror://hackage/${name}-${version}.tar.gz";
                inherit (hackage.${name}.${version}) sha256; };
            in let src = self.buildPackages.pkgs.runCommand "${name}-${version}-src" { } ''
                tmp=$(mktemp -d)
                cd $tmp
                tar xzf ${tarball}
                mv "${name}-${version}" $out
                '';
            in cabalProject' (builtins.removeAttrs args [ "version" ] // { inherit src; });

        # This function is like `cabalProject` but it makes the plan-nix available
        # separately from the hsPkgs.  The advantage is that the you can get the
        # plan-nix without building the project.
        cabalProject' =
            { ... }@args:
            let plan = importAndFilterProject (callCabalProjectToNix args);
            in let pkg-set = mkCabalProjectPkgSet
                { plan-pkgs = plan.pkgs;
                  pkg-def-extras = args.pkg-def-extras or [];
                  modules = (args.modules or [])
                          ++ self.lib.optional (args ? ghc) { ghc.package = args.ghc; };
                };
            in { inherit (pkg-set.config) hsPkgs; plan-nix = plan.nix; };

        cabalProject = args: let p = cabalProject' args;
            in p.hsPkgs // {
              inherit (p) plan-nix;
              # Provide `nix-shell -A shells.ghc` for users migrating from the reflex-platform.
              # But we should encourage use of `nix-shell -A shellFor`
              shells.ghc = p.hsPkgs.shellFor {};
            };

        stackProject' =
            { ... }@args:
            let stack = importAndFilterProject (callStackToNix ({ inherit cache; } // args));
                generatedCache = genStackCache {
                    inherit (args) src;
                    stackYaml = args.stackYaml or "stack.yaml";
                };
                cache = args.cache or generatedCache;
            in let pkg-set = mkStackPkgSet
                { stack-pkgs = stack.pkgs;
                  pkg-def-extras = (args.pkg-def-extras or []);
                  modules =  self.lib.singleton (mkCacheModule cache)
                             ++ (args.modules or [])
                             ++ self.lib.optional (args ? ghc) { ghc.package = args.ghc; };
                };
            in { inherit (pkg-set.config) hsPkgs; stack-nix = stack.nix; };

        stackProject = args: let p = stackProject' args;
            in p.hsPkgs // {
              inherit (p) stack-nix;
              # Provide `nix-shell -A shells.ghc` for users migrating from the reflex-platform.
              # But we should encourage use of `nix-shell -A shellFor`
              shells.ghc = p.hsPkgs.shellFor {};
            };

        # Like `cabalProject'`, but for building the GHCJS compiler.
        # This is exposed to allow GHCJS developers to work on the GHCJS
        # code in a nix-shell with `shellFor`.
        ghcjsProject = import ../lib/ghcjs-project.nix { pkgs = self; };

        # The functions that return a plan-nix often have a lot of dependencies
        # that could be GCed and also will not make it into hydra cache.
        # Use this `withInputs` function to make sure your tests include
        # the dependencies needed explicitly.  For example, if you have:
        #   project = cabalProject' {...};
        # In your tests module add something that is effectively
        #   testProjectPlan = withInputs project.plan-nix;
        withInputs = self.recurseIntoAttrs;
  
        # Add this to your tests to make all the dependencies of haskell.nix
        # are tested and cached.
        haskellNixRoots = self.recurseIntoAttrs {
          Level0 = haskellNixRoots' 0;
          Level1 = haskellNixRoots' 1;
        };

        haskellNixRoots' = ifdLevel:
            let filterSupportedGhc = self.lib.filterAttrs (n: _: n == "ghc865");
          in self.recurseIntoAttrs ({
            # Things that require no IFD to build
            inherit (self.buildPackages.haskell-nix) nix-tools source-pins;
            bootstap-nix-tools = self.buildPackages.haskell-nix.bootstrap.packages.nix-tools;
            alex-plan-nix = withInputs self.buildPackages.haskell-nix.bootstrap.packages.alex-project.plan-nix;
            happy-plan-nix = withInputs self.buildPackages.haskell-nix.bootstrap.packages.happy-project.plan-nix;
            hscolour-plan-nix = withInputs self.buildPackages.haskell-nix.bootstrap.packages.hscolour-project.plan-nix;
          } // self.lib.optionalAttrs (ifdLevel > 0) {
            # Things that require one IFD to build (the inputs should be in level 0)
            alex = self.buildPackages.haskell-nix.bootstrap.packages.alex;
            happy = self.buildPackages.haskell-nix.bootstrap.packages.happy;
            hscolour = self.buildPackages.haskell-nix.bootstrap.packages.hscolour;
            ghc865 = self.buildPackages.haskell-nix.compiler.ghc865;
            ghc-extra-projects = self.recurseIntoAttrs (builtins.mapAttrs (_: proj: withInputs proj.plan-nix)
              (filterSupportedGhc self.ghc-extra-projects));
          } // self.lib.optionalAttrs (ifdLevel > 1) {
            # Things that require two levels of IFD to build (inputs should be in level 1)
            inherit (self.haskell-nix) nix-tools;
            ghc-extra-packages = self.recurseIntoAttrs
              (filterSupportedGhc self.ghc-extra-packages);
          });
    };
}
