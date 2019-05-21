{ pkgs ? import nixpkgs {}
# Use a pinned nixpkgs rather than the one on NIX_PATH
, nixpkgs ? ./nixpkgs

# You can provide different pins for hackage.nix and stackage.nix if required.
# It's also possible to override these sources with NIX_PATH.
, hackageSourceJSON ? ./hackage-src.json
, stackageSourceJSON ? ./stackage-src.json
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

  # Utility function for downloading a pinned git repo, that can be
  # overridden with NIX_PATH.
  fetchExternal = import ./lib/fetch-external.nix;

  cleanSourceHaskell = pkgs.callPackage ./lib/clean-source-haskell.nix {};

  # All packages from Hackage as Nix expressions
  hackage = import (fetchExternal {
    name     = "hackage-exprs-source";
    specJSON = hackageSourceJSON;
    override = "hackage";
  });

  # The set of all Stackage snapshots
  stackage = import (fetchExternal {
    name     = "stackage-snapshot-source";
    specJSON = stackageSourceJSON;
    override = "stackage";
  });

  packages = self: ({
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
        pkg-def = stackage.${stack-pkgs.resolver};
        # The compiler referenced in the stack config
        compiler = (stack-pkgs.extras hackage).compiler or (pkg-def hackage).compiler;
        patchesModule = ghcHackagePatches.${compiler.nix-name} or {};
      in self.mkPkgSet {
        inherit pkg-def;
        pkg-def-extras = [ stack-pkgs.extras ] ++ pkg-def-extras;
        modules = [ patchesModule ] ++ modules;
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
      in self.mkPkgSet {
        inherit pkg-def;
        pkg-def-extras = [ plan-pkgs.extras ] ++ pkg-def-extras;
        modules = [ ghcHackagePatches.${compiler.nix-name} ] ++ modules;
      };

    # Programs for generating Nix expressions from Cabal and Stack
    # files. We need to make sure we build this from the buildPackages,
    # we never want to actually cross compile nix-tools on it's own.
    nix-tools = pkgs.buildPackages.callPackage ./nix-tools { inherit fetchExternal cleanSourceHaskell; inherit (self) mkCabalProjectPkgSet; };

    # Function to call stackToNix
    callStackToNix = self.callPackage ./call-stack-to-nix.nix {};

    # Snapshots of Hackage and Stackage, converted to Nix expressions,
    # regularly updated.
    inherit hackage stackage;

    # Scripts for keeping Hackage and Stackage up to date.
    maintainer-scripts = {
      update-hackage = self.callPackage ./scripts/update-hackage.nix {};
      update-stackage = self.callPackage ./scripts/update-stackage.nix {};
      update-pins = self.callPackage ./scripts/update-pins.nix {};
    };

    # Make this handy overridable fetch function available.
    inherit fetchExternal;

    # Function for cleaning haskell source diretories.
    inherit cleanSourceHaskell;

    # Produce a fixed output derivation from a moving target (hackage index tarball)
    hackageTarball = { index-state, sha256 }:
      pkgs.runCommand "01-index.tar.gz-at-${builtins.replaceStrings [":"] [""] index-state}" {
        nativeBuildInputs = [ pkgs.curl ];
        outputHashAlgo = "sha256";
        outputHash = sha256;
        # We'll use fetchurl's result in an env var ...
        HACKAGE_INDEX = builtins.fetchurl "https://hackage.haskell.org/01-index.tar.gz";
        # ... and mark that impure.  That way we can
        # ensure the sore path stays the same and doesn't
        # depend on the fetchurl result path.
        impureEnvVars = [ "HACKAGE_INDEX" ];
      }
      ''
      ${self.nix-tools}/bin/truncate-index -o $out -i $HACKAGE_INDEX -s ${index-state}
      '';

    mkLocalHackageRepo = import ./mk-local-hackage-repo { inherit (self) hackageTarball; inherit pkgs; };

    dotCabal = { index-state, sha256 }@args:
      pkgs.runCommand "dot-cabal-at-${builtins.replaceStrings [":"] [""] index-state}" { nativeBuildInputs = [ pkgs.cabal-install ]; } ''
        mkdir -p $out/.cabal
        cat <<EOF > $out/.cabal/config
        repository cached
          url: file:${self.mkLocalHackageRepo args}
          secure: True
          root-keys:
          key-threshold: 0
        EOF
        mkdir -p $out/.cabal/packages/cached
        HOME=$out cabal new-update cached
      '';


    # Takes a haskell src directory runs cabal new-configure and plan-to-nix.
    # Resulting nix files are added to nix-plan subdirectory.
    callCabalProjectToNix = import ./lib/cabalProjectToNix.nix {
      inherit (self) dotCabal;
      inherit pkgs;
      inherit (pkgs) runCommand cabal-install ghc;
      inherit (pkgs.haskellPackages) hpack;
      inherit (self) nix-tools;
      inherit (pkgs) symlinkJoin;
    };
  });

in
  pkgs.lib.makeScope pkgs.newScope packages
