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
      exe="${packages.test-project-flags.components.exes.test-project-flags-exe}/bin/test-project-flags-exe"

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
