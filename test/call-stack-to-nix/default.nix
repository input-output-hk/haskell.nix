{ stdenv, mkStackPkgSet, callStackToNix }:

with stdenv.lib;

let
  pkgSet = mkStackPkgSet {
    stack-pkgs = builtins.trace "Calling callStackToNix!" (import (callStackToNix {
      src = ../stack-simple;
    }));
    pkg-def-extras = [];
    modules = [];
  };
  packages = pkgSet.config.hsPkgs;
in
  stdenv.mkDerivation {
    name = "callStackToNix-test";

    buildCommand = ''
      exe="${packages.stack-simple.components.exes.stack-simple-exe}/bin/stack-simple-exe"

      printf "checking whether executable runs... " >& 2
      $exe

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  }
