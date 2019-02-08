{ pkgs ? import <nixpkgs> {}
}:

let
  # pkg-def's may reference boot packages, but those
  # are not guaranteed to be available on hackage, as
  # it is a manual process.  They might eventually show
  # up much later on hackage; but are not installable
  # anyway. Therefore we just strip them out of the
  # pkg-def's packages.
  boot-pkgs = [ "rts" "ghc" "ghci" "ghc-boot" "ghc-boot-th"
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

  # All packages from Hackage as Nix expressions
  hackage = import (fetchExternal {
    name     = "hackage-exprs-source";
    specJSON = ./hackage-src.json;
    override = "hackage";
  });

  # The set of all Stackage snapshots
  stackage = import (fetchExternal {
    name     = "stackage-snapshot-source";
    specJSON = ./stackage-src.json;
    override = "stackage";
  });

  packages = self: ({
    # Utility functions for working with the component builder.
    haskellLib = let hl = import ./lib { inherit (pkgs) lib; haskellLib = hl; }; in hl;

    # Create a Haskell package set based on a cabal build plan (plan-to-nix)
    # and Nix expressions representing cabal packages (cabal-to-nix).
    mkPkgSet =
      { pkg-def  # Base package set. Either from stackage (via stack-to-nix) or from a cabal projects plan file (via plan-to-nix)
      , pkg-def-overlays ? [] # Additional packages to augment the Base package set `pkg-def` with.
      , modules ? []
      }@args:

      import ./package-set.nix (args // {
        inherit hackage pkgs;
        pkg-def = strip-pkg-def pkgs pkg-def;
      });

    # Create a Haskell package set based on a Stack configuration.
    mkStackPkgSet =
      { stack-pkgs  # Path to the output of stack-to-nix
      , pkg-def-overlays ? []
      , modules ? []
      }@args:

      let
        # The Stackage release referenced in the stack config
        pkg-def = stackage.${stack-pkgs.resolver};
        # The compiler referenced in the stack config
        compiler = (stack-pkgs.overlay hackage).compiler or (pkg-def hackage).compiler;
      in self.mkPkgSet {
        inherit pkg-def;
        pkg-def-overlays = [ stack-pkgs.overlay ] ++ pkg-def-overlays;
        modules = [ ghcHackagePatches.${compiler.nix-name} ] ++ modules;
      };

    # Programs for generating Nix expressions from Cabal and Stack
    # files.
    nix-tools = self.callPackage ./nix-tools {
      inherit fetchExternal;
    };

    # Snapshots of Hackage and Stackage, converted to Nix expressions,
    # regularly updated.
    inherit hackage stackage;
  });

in
  pkgs.lib.makeScope pkgs.newScope packages
