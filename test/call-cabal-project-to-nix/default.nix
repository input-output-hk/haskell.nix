{ stdenv, lib, buildPackages, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject, recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  # This test could use cabalProject', but it does so that it
  # tests using callCabalProjectToNix and importAndFilterProject
  callProjectResults = callCabalProjectToNix {
    inherit compiler-nix-name evalPackages;
    # reuse the cabal-simple test project
    src = testSrc "cabal-simple";
    cabalProjectLocal = optionalString (__compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.8.0" >= 0) ''
      allow-newer: *:*

      repository head.hackage.ghc.haskell.org
        url: https://ghc.gitlab.haskell.org/head.hackage/
        secure: True
        key-threshold: 3
        root-keys:
          f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89
          26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329
          7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d
        --sha256: sha256-WrhyfhCN6IiP5/yCyQibeHHfUDoAUioAH2ysMLoRqdg=

      active-repositories: hackage.haskell.org, head.hackage.ghc.haskell.org:override
    '';
  };
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = importAndFilterProject {
      inherit (callProjectResults) projectNix sourceRepos src;
    };
    modules = [{ inherit evalPackages; }];
  };
  packages = pkgSet.config.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    plan-nix = callProjectResults.projectNix;
  };
  run = stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple.exePath}"

      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.cabal-simple.components.exes.cabal-simple}/test-stdout

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isGhcjs && __compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.6.1" >= 0;
      disabled = broken;
    };

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
    };
  };
}
