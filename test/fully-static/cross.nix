{ pkgs ? import nixpkgs { }
, nixpkgs ? import (../../nix/sources.nix).nixpkgs
}:

let
  pkgs' = pkgs.pkgsCross.musl64;
  haskellMusl64 = pkgs.callPackage ../.. { pkgs = pkgs'; };

  test = haskellMusl64.callPackage ./default.nix {
    inherit (pkgs') buildPackages;
  };

in {
  inherit (test) pandoc-gmp pandoc-integer-simple;
}
