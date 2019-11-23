{ stdenv, mkStackPkgSet, callStackToNix, importAndFilterProject, recurseIntoAttrs }:

with stdenv.lib;

let
  stack = importAndFilterProject (callStackToNix {
    src = ../stack-simple;
  });
  pkgSet = mkStackPkgSet {
    stack-pkgs = stack.pkgs;
    pkg-def-extras = [];
    modules = [];
  };
  packages = pkgSet.config.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = recurseIntoAttrs {
    stack-nix = stack.nix;
  };
  run = stdenv.mkDerivation {
    name = "callStackToNix-test";

    buildCommand = ''
      exe="${packages.stack-simple.components.exes.stack-simple-exe}/bin/stack-simple-exe${stdenv.hostPlatform.extensions.executable}"

      printf "checking whether executable runs... " >& 2
      cat ${packages.stack-simple.components.exes.stack-simple-exe.run}

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  };
}