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

        # Function for cleaning haskell source directories pulled from iohk-nix
        cleanSourceHaskell = import ../lib/clean-source-haskell.nix { pkgs = self; };

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
            inherit (self) lib runCommand;
            git = self.buildPackages.git;
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
            in mkPkgSet {
                inherit pkg-def;
                pkg-def-extras = [ stack-pkgs.extras
                                   self.ghc-boot-packages.${compiler.nix-name}
                                 ]
                              ++ pkg-def-extras;
                # set doExactConfig = true. The stackage set should be consistent
                # and we should trust stackage here!
                modules = [ { doExactConfig = true; } patchesModule ]
                       ++ modules
                       ++ stack-pkgs.modules;
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
                pkg-def-extras = [ plan-pkgs.extras ] ++ pkg-def-extras;
                # set doExactConfig = true, as we trust cabals resolution for
                # the plan.
                modules = [ { doExactConfig = true; } patchesModule ] ++ modules;
            };

        # Package sets for all stackage snapshots.
        snapshots = import ../snapshots.nix { inherit (self) lib; inherit mkPkgSet stackage; };
        # Pick a recent LTS snapshot to be our "default" package set.
        haskellPackages = snapshots."lts-13.26";

        # Programs for generating Nix expressions from Cabal and Stack
        # files. This version of nix-tools may be cross compiled.
        # We probably never want to actually cross compile nix-tools on
        # it's own.
        nix-tools-cross-compiled = self.lib.makeOverridable (import ../nix-tools) {
            inherit (self) pkgs lib symlinkJoin makeWrapper
                           git nix nix-prefetch-git;
            inherit (self.haskell-nix) fetchExternal cleanSourceHaskell mkCabalProjectPkgSet;
            hpack = self.haskell.lib.justStaticExecutables
                (self.haskellPackages.hpack);
        };
        # While `nix-tools-cross-compiled` may be cross compiled,
        # getting it from `buildPackages` we should get
        # nix-tools suitable for running on the build system.
        nix-tools = self.buildPackages.haskell-nix.nix-tools-cross-compiled;
        # TODO perhaps there is a cleaner way to get a suitable nix-tools.

            # Produce a fixed output derivation from a moving target (hackage index tarball)
        hackageTarball = { index-state, sha256, nix-tools ? self.nix-tools, ... }:
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

        update-index-state-hashes = import ../scripts/update-index-state-hashes.nix {
            inherit (self.haskell-nix) indexStateHashesPath nix-tools;
            inherit (self) coreutils nix writeShellScriptBin stdenv;
        };

        # Function to call stackToNix
        callStackToNix = import ../lib/call-stack-to-nix.nix {
            pkgs = self.buildPackages.pkgs;
            inherit (self.buildPackages.pkgs) runCommand;
            inherit (self.buildPackages.haskell-nix) nix-tools;
        };

        # Takes a haskell src directory runs cabal new-configure and plan-to-nix.
        # Resulting nix files are added to nix-plan subdirectory.
        callCabalProjectToNix = import ../lib/call-cabal-project-to-nix.nix {
            index-state-hashes = import indexStateHashesPath;
            inherit (self.buildPackages.haskell-nix) dotCabal nix-tools haskellLib;
            pkgs = self.buildPackages.pkgs;
            inherit (self.buildPackages.haskell-nix.haskellPackages.hpack.components.exes) hpack;
            inherit (self.buildPackages.pkgs) runCommand cabal-install ghc symlinkJoin cacert;
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
        hackage-package =
            { name
            , version
            , index-state ? builtins.trace "Using latest index state!"  self.lib.last (builtins.attrNames (import indexStateHashesPath))
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
            in (cabalProject (builtins.removeAttrs args [ "name" "version" ] // { inherit index-state src; })).${name};

        cabalProject' =
            { index-state ? builtins.trace "Using latest index state!"  self.lib.last (builtins.attrNames (import indexStateHashesPath))
            , ... }@args:
            let plan = (importAndFilterProject (callCabalProjectToNix
                                    (builtins.trace "Using index-state: ${index-state}"
                                     (args // { inherit index-state; }))));
            in let pkg-set = mkCabalProjectPkgSet
                { plan-pkgs = plan.pkgs;
                  pkg-def-extras = args.pkg-def-extras or [];
                  modules = (args.modules or [])
                          ++ self.lib.optional (args ? ghc) { ghc.package = args.ghc; };
                };
            in { inherit (pkg-set.config) hsPkgs; plan-nix = plan.nix; };

        cabalProject = args: let p = cabalProject' args;
            in p.hsPkgs // { inherit (p) plan-nix; };

        stackProject =
            { ... }@args:
            let stack-pkgs = (importAndFilterProject (callStackToNix args)).pkgs;
            in let pkg-set = mkStackPkgSet
                { inherit stack-pkgs;
                  pkg-def-extras = (args.pkg-def-extras or []);
                  modules = (args.modules or [])
                          ++ self.lib.optional (args ? ghc) { ghc.package = args.ghc; };
                };
            in pkg-set.config.hsPkgs;
    };
}
