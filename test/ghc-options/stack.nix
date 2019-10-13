{ stdenv, mkStackPkgSet, callStackToNix, importAndFilterProject }:

with stdenv.lib;

let
  stack = (importAndFilterProject (callStackToNix {
    src = ./.;
  }));
  pkgSet = mkStackPkgSet {
    stack-pkgs = stack.pkgs;
  };
  packages = pkgSet.config.hsPkgs;
in
  stdenv.mkDerivation {
    name = "callStackToNix-test";

    buildCommand = ''
      exe="${packages.test-ghc-options.components.exes.test-ghc-options-exe}/bin/test-ghc-options-exe"

      printf "checking whether executable runs... " >& 2
      $exe

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
      stack-nix = stack.nix;
    };
  }
