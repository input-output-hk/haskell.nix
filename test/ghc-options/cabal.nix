{ stdenv, lib, cabalProject', recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "ghc-options";
    # TODO find a way to get the ghc-options into plan.json so we can use it in plan-to-nix
    modules = [ {
      packages.test-ghc-options.ghcOptions = ["-DTEST_GHC_OPTION"];

      # This should also work here
      #   ghcOptions = ["-DTEST_GHC_OPTION"];
      # or this
      #   packages.test-ghc-options.components.library.ghcOptions = ["-DTEST_GHC_OPTION"];
      #   packages.test-ghc-options.components.exes.test-ghc-options-exe.ghcOptions = ["-DTEST_GHC_OPTION"];
    } ];
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "ghc-options-cabal-test";

    buildCommand = ''
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.test-ghc-options.components.exes.test-ghc-options-exe}/test-stdout

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isGhcjs && __elem compiler-nix-name ["ghc961" "ghc962"];
      disabled = broken;
    };

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
}
