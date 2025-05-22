{ stdenv, lib, stackProject', recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = stackProject' {
    inherit evalPackages;
    src = testSrc "ghc-options";
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  # This test is somehow broken for ghcjs
  meta.disabled = stdenv.hostPlatform.isGhcjs || compiler-nix-name != "ghc984";

  ifdInputs = {
    inherit (project) stack-nix;
  };
  run = stdenv.mkDerivation {
    name = "callStackToNix-test";

    buildCommand = ''
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.test-ghc-options.components.exes.test-ghc-options-exe}/test-stdout

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isGhcjs && __elem compiler-nix-name ["ghc961"];
      disabled = broken;
    };

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
}
