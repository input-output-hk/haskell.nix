{ pkgs ? import <nixpkgs> {}

# a different haskell infrastructure
, haskell ? import haskellLibSrc { inherit pkgs; }
, haskellLibSrc ? pkgs.fetchFromGitHub {
    owner  = "input-output-hk";
    repo   = "haskell.nix";
    rev    = "0de60e8b0cd0338b82d25d6148246d1cdbf37a47";
    sha256 = "0j3mjlzwg8f95rsnj1vrcyivil65is16i48l1c3czm0ps39frch9";
    name   = "haskell-lib-source";
  }
}:

let
  hsPkgs = import ./pkgs.nix { inherit (haskell) mkPkgSet; };
in
  hsPkgs // {
    nix-tools-all-execs = pkgs.symlinkJoin
      { name = "nix-tools";
        paths = builtins.attrValues hsPkgs.nix-tools.components.exes; };
  }
