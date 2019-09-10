let
  # Utility function for downloading a pinned git repo, that can be
  # overridden with NIX_PATH.
  fetchExternal = import ./lib/fetch-external.nix;
in
{ pkgs ? import nixpkgs nixpkgsArgs
# Use a pinned nixpkgs rather than the one on NIX_PATH
, nixpkgs ? ./nixpkgs
# Provide args to the nixpkgs instantiation.
, nixpkgsArgs ? {}
# You can provide different pins for hackage.nix and stackage.nix if required.
# It's also possible to override these sources with NIX_PATH.
, hackageSourceJSON ? ./hackage-src.json
, stackageSourceJSON ? ./stackage-src.json
, hackageSrc ? fetchExternal {
    name     = "hackage-exprs-source";
    specJSON = hackageSourceJSON;
    override = "hackage";
  }
}:

let
  # pkg-def's may reference boot packages, but those
  # are not guaranteed to be available on hackage, as
  # it is a manual process.  They might eventually show
  # up much later on hackage; but are not installable
  # anyway. Therefore we just strip them out of the
  # pkg-def's packages.
  #
  # Note: these will need to be provided by alternative
  #       means outside of hackage.
  boot-pkgs = [ "rts" "ghc" "ghc-boot-th" "ghc-boot" "ghci"
                "ghc-heap" # since ghc 8.6.
                "iserv" "libiserv" "iserv-proxy"
              ];
  strip-pkg-def = pkgs: pkg-def: hackage: with pkgs.lib;
    mapAttrs (k: v: if k == "packages"
                    then filterAttrs (k: _: !(builtins.elem k boot-pkgs)) v
                    else v)
             (pkg-def hackage);

  # ghc hackage patches.
  # these are patches that turn hackage packages into the same as the ones
  # ghc ships with the supposedly same version. See GHC Track Issue: 16199
  ghcHackagePatches = import ./patches;

  compat = import ./lib/compat.nix;

  # Function for cleaning haskell source directories pulled from iohk-nix
  cleanSourceHaskell = pkgs.callPackage ./lib/clean-source-haskell.nix {};

  # All packages from Hackage as Nix expressions
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

  packages = pkgs: self: (rec {
    inherit pkgs; # Make pkgs available (it is the underlying nixpkgs)

     # Packages built to run on the build platform, not the host platform
    buildPackages = pkgs.buildPackages.lib.makeScope pkgs.buildPackages.newScope
      (packages pkgs.buildPackages);

    # Utility functions for working with the component builder.
    haskellLib = let hl = import ./lib { inherit (pkgs) lib; haskellLib = hl; }; in hl;

    # Create a Haskell package set based on a cabal build plan (plan-to-nix)
    # and Nix expressions representing cabal packages (cabal-to-nix).
    mkPkgSet =
      { pkg-def  # Base package set. Either from stackage (via stack-to-nix) or from a cabal projects plan file (via plan-to-nix)
      , pkg-def-extras ? [] # Additional packages to augment the Base package set `pkg-def` with.
      , modules ? []
      }@args:

      import ./package-set.nix (args // {
        inherit hackage pkgs;
        pkg-def = strip-pkg-def pkgs pkg-def;
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
      in self.mkPkgSet {
        inherit pkg-def;
        pkg-def-extras = [ stack-pkgs.extras ] ++ pkg-def-extras;
        # set doExactConfig = true. The stackage set should be consistent
        # and we should trust stackage here!
        modules = [ { doExactConfig = true; } patchesModule ] ++ modules;
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
      in self.mkPkgSet {
        inherit pkg-def;
        pkg-def-extras = [ plan-pkgs.extras ] ++ pkg-def-extras;
        # set doExactConfig = true, as we trust cabals resolution for
        # the plan.
        modules = [ { doExactConfig = true; } patchesModule ] ++ modules;
      };

    # Package sets for all stackage snapshots.
    snapshots = self.callPackage ./snapshots.nix {};
    # Pick a recent LTS snapshot to be our "default" package set.
    haskellPackages = self.snapshots."lts-13.26";

    # Programs for generating Nix expressions from Cabal and Stack
    # files. This version of nix-tools may be cross compiled.
    # We probably never want to actually cross compile nix-tools on
    # it's own.
    nix-tools-cross-compiled = pkgs.callPackage ./nix-tools {
      inherit fetchExternal cleanSourceHaskell;
      hpack = pkgs.haskell.lib.justStaticExecutables
        (pkgs.haskellPackages.hpack);
      inherit (self) mkCabalProjectPkgSet;
    };
    # While `nix-tools-cross-compiled` may be cross compiled,
    # getting it from `buildPackages` we should get
    # nix-tools suitable for running on the build system.
    nix-tools = buildPackages.nix-tools-cross-compiled;
    # TODO perhaps there is a cleaner way to get a suitable nix-tools.

    # Snapshots of Hackage and Stackage, converted to Nix expressions,
    # regularly updated.
    inherit hackageSrc stackageSrc;
    inherit hackage stackage;
    inherit indexStateHashesPath;

    # Make this handy overridable fetch function available.
    inherit fetchExternal;

    # Function for cleaning haskell source diretories.
    inherit cleanSourceHaskell;

    # Produce a fixed output derivation from a moving target (hackage index tarball)
    hackageTarball = { index-state, sha256 }:
      assert sha256 != null;
      pkgs.fetchurl {
        name = "01-index.tar.gz-at-${builtins.replaceStrings [":"] [""] index-state}";
        url = "https://hackage.haskell.org/01-index.tar.gz";
        downloadToTemp = true;
        postFetch = "${self.nix-tools}/bin/truncate-index -o $out -i $downloadedFile -s ${index-state}";

        outputHashAlgo = "sha256";
        outputHash = sha256;
      };

    mkLocalHackageRepo = import ./mk-local-hackage-repo { inherit (self) hackageTarball; inherit pkgs; };

    dotCabal = { index-state, sha256, cabal-install ? pkgs.cabal-install }@args:
      pkgs.runCommand "dot-cabal-at-${builtins.replaceStrings [":"] [""] index-state}" { nativeBuildInputs = [ cabal-install ]; } ''
        mkdir -p $out/.cabal
        cat <<EOF > $out/.cabal/config
        repository cached
          url: file:${self.mkLocalHackageRepo
            (builtins.removeAttrs args ["cabal-install"])}
          secure: True
          root-keys:
          key-threshold: 0
        EOF
        mkdir -p $out/.cabal/packages/cached
        HOME=$out cabal new-update cached
      '';

    update-index-state-hashes = self.callPackage ./scripts/update-index-state-hashes.nix {};

    # Function to call stackToNix
    callStackToNix = import ./lib/call-stack-to-nix.nix {
      pkgs = buildPackages.pkgs;
      inherit (buildPackages.pkgs) runCommand;
      inherit (buildPackages) nix-tools;
    };

    # Takes a haskell src directory runs cabal new-configure and plan-to-nix.
    # Resulting nix files are added to nix-plan subdirectory.
    callCabalProjectToNix = import ./lib/call-cabal-project-to-nix.nix {
      index-state-hashes = import indexStateHashesPath;
      inherit (buildPackages) dotCabal haskellLib;
      pkgs = buildPackages.pkgs;
      inherit (buildPackages.pkgs.haskellPackages) hpack;
      inherit (buildPackages.pkgs) runCommand cabal-install ghc symlinkJoin cacert;
      inherit (buildPackages) nix-tools;
    };

    # Loads a plan and filters the package directories using cleanSourceWith
    importAndFilterProject = import ./lib/import-and-filter-project.nix {
      inherit pkgs haskellLib;
    };

    # References to the unpacked sources, for caching in a Hydra jobset.
    source-pins = self.callPackage ./lib/make-source-pins.nix {
      sources = [ hackageSrc stackageSrc pkgs.path ];
    };

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
      , index-state ? builtins.trace "Using latest index state!"  pkgs.lib.last (builtins.attrNames (import indexStateHashesPath))
      }:
      let tarball = pkgs.fetchurl {
        url = "mirror://hackage/${name}-${version}.tar.gz";
        inherit (hackage.${name}.${version}) sha256; };
      in let src = buildPackages.pkgs.runCommand "${name}-${version}-src" { } ''
        tmp=$(mktemp -d)
        cd $tmp
        tar xzf ${tarball}
        mv "${name}-${version}" $out
        '';
      in let plan-pkgs = (callCabalProjectToNix { inherit src; index-state = builtins.trace "Using index-state: ${index-state}" index-state; }).pkgs;
      in let pkg-set = mkCabalProjectPkgSet { inherit plan-pkgs; };
      in pkg-set.config.hsPkgs.${name};
  });

in
  pkgs.lib.makeScope pkgs.newScope (packages pkgs)
