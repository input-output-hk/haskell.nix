{ stdenv, mkStackPkgSet, callStackToNix, importAndFilterProject, recurseIntoAttrs, haskellLib, testSrc }:

with stdenv.lib;

let
  callProjectResults = callStackToNix {
    src = testSrc "stack-simple";
  };
  pkgSet = mkStackPkgSet {
    stack-pkgs = importAndFilterProject callProjectResults;
    pkg-def-extras = [];
    modules = [];
  };
  packages = pkgSet.config.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    stack-nix = callProjectResults.projectNix;
  };
  run = stdenv.mkDerivation {
    name = "callStackToNix-test";

    buildCommand = ''
      exe="${packages.stack-simple.components.exes.stack-simple-exe}/bin/stack-simple-exe${stdenv.hostPlatform.extensions.executable}"

      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.stack-simple.components.exes.stack-simple-exe}/test-stdout

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  };
}
