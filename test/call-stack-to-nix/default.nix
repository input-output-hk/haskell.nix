{ stdenv, lib, mkStackPkgSet, callStackToNix, importAndFilterProject, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  callProjectResults = callStackToNix {
    src = testSrc "stack-simple";
    inherit evalPackages;
  };
  pkgSet = mkStackPkgSet {
    stack-pkgs = importAndFilterProject callProjectResults;
    pkg-def-extras = [];
    modules = [{inherit evalPackages;}];
  };
  packages = pkgSet.config.hsPkgs;

in lib.recurseIntoAttrs {
  meta.disabled = compiler-nix-name != "ghc865";
  ifdInputs = {
    stack-nix = callProjectResults.projectNix;
  };
  run = stdenv.mkDerivation {
    name = "callStackToNix-test";

    buildCommand = ''
      exe="${packages.stack-simple.components.exes.stack-simple-exe.exePath}"

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
