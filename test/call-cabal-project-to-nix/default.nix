{ stdenv, lib, buildPackages, mkCabalProjectPkgSet, callCabalProjectToNix, loadCabalPlan, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  # Android links every exe statically so qemu-user can run it on the
  # build host (a dynamic Android binary references /system/bin/linker64,
  # which the build host doesn't ship).  modules/cabal-project.nix sets
  # this for normal projects, but this test deliberately builds via the
  # low-level callCabalProjectToNix / mkCabalProjectPkgSet path, which
  # doesn't pull that module's config in — so replicate the relevant
  # default here, for both the plan (callCabalProjectToNix) and the build
  # (modules), so cabal records and applies the same flags.
  androidStaticLocal = lib.optionalString stdenv.hostPlatform.isAndroid ''
    package *
      ghc-options: -optl-static -optl-ldl${lib.optionalString stdenv.hostPlatform.isAarch32 " -optl-no-pie"}
  '';
  # This test could use cabalProject', but it does so that it
  # tests using callCabalProjectToNix and importAndFilterProject
  callProjectResults = callCabalProjectToNix {
    inherit compiler-nix-name evalPackages;
    # reuse the cabal-simple test project
    src = testSrc "cabal-simple";
    cabalProjectLocal = builtins.readFile ../cabal.project.local
      + androidStaticLocal
      + lib.optionalString (haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64) ''
        constraints: text -simdutf, text source
    '';
  };
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = loadCabalPlan {
      inherit callProjectResults;
      selectedCompiler = buildPackages.haskell-nix.compiler.${compiler-nix-name};
    };
    inherit compiler-nix-name;
    inherit (callProjectResults) extra-hackages;
    modules = [{
      inherit evalPackages;
      compiler.nix-name = compiler-nix-name;
    }] ++ lib.optional (androidStaticLocal != "") {
      cabalProjectLocal = lib.mkBefore androidStaticLocal;
    };
  };
  packages = pkgSet.config.hsPkgs;

in lib.recurseIntoAttrs {
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
