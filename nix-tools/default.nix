{ lib, symlinkJoin, makeWrapper
, hpack, git, nix, nix-prefetch-scripts
, fetchExternal, cleanSourceHaskell, mkCabalProjectPkgSet }:

let
  src = cleanSourceHaskell (fetchExternal {
    name     = "nix-tools-src";
    specJSON = ./nix-tools-src.json;
    override = "nix-tools";
  });

  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
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
    ];
  };

  hsPkgs = pkgSet.config.hsPkgs;

  tools = [ hpack git nix nix-prefetch-scripts ];
in
  symlinkJoin {
    name = "nix-tools";
    paths = builtins.attrValues hsPkgs.nix-tools.components.exes;
    buildInputs = [ makeWrapper ];
    postBuild = ''
      for prog in stack-to-nix cabal-to-nix plan-to-nix; do
        wrapProgram "$out/bin/$prog" --prefix PATH : "${lib.makeBinPath tools}"
      done
    '';
  }
