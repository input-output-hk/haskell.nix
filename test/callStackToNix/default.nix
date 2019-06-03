{ runCommand, lib, mkStackPkgSet, callStackToNix }:

let
  pkgSet = mkStackPkgSet {
    stack-pkgs = callStackToNix { src = ./.; };
    pkg-def-extras = [];
    modules = [];
  };

  packages = pkgSet.config.hsPkgs;

in
  runCommand "callStackToNix-test" {
    meta.platforms = lib.platforms.all;
    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  } ''
    exe="${packages.stack-simple.components.exes.stack-simple-exe}/bin/stack-simple-exe"

    printf "checking whether executable runs... " >& 2
    $exe

    touch $out
  ''
