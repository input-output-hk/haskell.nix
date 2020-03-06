{ pkgs, lib, symlinkJoin, makeWrapper
, hpack, git, nix, nix-prefetch-git
, fetchExternal, cleanSourceHaskell, mkCabalProjectPkgSet
, ... }@args:

let
  src = cleanSourceHaskell {
    src = fetchExternal {
      name     = "nix-tools-src";
      specJSON = ./nix-tools-src.json;
      override = "nix-tools-src";
    };
  };

  # we need to patch Cabal, as its data directory logic is broken for component builds, which haskell.nix
  # uses excessively. See issue https://github.com/haskell/cabal/issues/5862, and the fix for Cabal 3.0
  # in https://github.com/haskell/cabal/pull/6055. We apply the haskell/cabal#6055 here.
  cabalPatch = pkgs.fetchpatch {
    url = "https://patch-diff.githubusercontent.com/raw/haskell/cabal/pull/6055.diff";
    sha256 = "145g7s3z9q8d18pxgyngvixgsm6gmwh1rgkzkhacy4krqiq0qyvx";
    stripLen = 1;
  };

  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = if args ? ghc
                then import (./pkgs + "-${args.ghc.version}.nix")
                else import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [
      {
        packages.transformers-compat.components.library.doExactConfig = true;
        packages.time-compat.components.library.doExactConfig = true;
        packages.time-locale-compat.components.library.doExactConfig = true;
      }

      {
        packages.nix-tools.src = src;
      }

      {
        packages.Cabal.patches = [ cabalPatch ];
      }

      {
        # Make Cabal reinstallable
        nonReinstallablePkgs =
          [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
            "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
            "ghc-boot"
            "ghc" "Win32" "array" "binary" "bytestring" "containers"
            "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
            "hpc"
            "mtl" "parsec" "process" "text" "time" "transformers"
            "unix" "xhtml"
          ];
      }
    ] ++ pkgs.lib.optional (args ? ghc) { ghc.package = args.ghc; };
  };

  hsPkgs = pkgSet.config.hsPkgs;

  tools = [ hpack git nix nix-prefetch-git ];
in
  symlinkJoin {
    name = "nix-tools";
    paths = builtins.attrValues hsPkgs.nix-tools.components.exes;
    buildInputs = [ makeWrapper ];
    meta.platforms = lib.platforms.all;
    # We wrap the -to-nix executables with the executables from `tools` (e.g. nix-prefetch-git)
    # so that consumers of `nix-tools` won't have to provide those tools.
    postBuild = ''
      for prog in stack-to-nix cabal-to-nix plan-to-nix; do
        wrapProgram "$out/bin/$prog" --prefix PATH : "${lib.makeBinPath tools}"
      done
    '';
  }
