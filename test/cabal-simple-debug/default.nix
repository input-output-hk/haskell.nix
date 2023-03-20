# Test a package set
{ stdenv, lib, util, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, dwarfdump }:

with lib;

let
  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-simple-debug";
    cabalProject = ''
      packages: .
      allow-newer: aeson:*
    '' + lib.optionalString (__elem compiler-nix-name ["ghc96020230302" "ghc961"]) ''
      allow-newer: *:base, *:ghc-prim, *:template-haskell
    '';
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  # DWARF only works on linux with GHC 8.10.2 and newer
  # GHC 9.2.1 disabled because of https://github.com/input-output-hk/haskell.nix/issues/1332
  meta.disabled = __elem compiler-nix-name ["ghc865" "ghc884" "ghc921" "ghc922" "ghc923" "ghc924" "ghc925" "ghc926" "ghc927"]
    || !stdenv.hostPlatform.isLinux || haskellLib.isCrossHost || stdenv.hostPlatform.isMusl || stdenv.hostPlatform.isAarch64;
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "cabal-simple-debug-test";

    buildCommand = ''
      exe="${(packages.cabal-simple.components.exes.cabal-simple.dwarf).exePath}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme:
      printf "checking whether executable included DWARF debug info... " >& 2
      (${dwarfdump}/bin/dwarfdump $exe || true) | grep -c 'libraries/base/[A-Za-z0-9/]*\.hs'
      (${dwarfdump}/bin/dwarfdump $exe || true) | grep -c '\/Main\.hs'

      touch $out
    '';

    meta = {
      platforms = platforms.all;
    };

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
