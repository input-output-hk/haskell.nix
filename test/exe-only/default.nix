# Test a package set
{ stdenv, lib, util, haskell-nix, recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = haskell-nix.cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "exe-only";
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "exe-only-test";

    buildCommand = ''
      exe="${packages.exe-only.components.exes.exe-only.exePath}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable ran... " >& 2
      cat ${haskellLib.check packages.exe-only.components.exes.exe-only}/test-stdout
    '' +
    # Aarch are statically linked and does not have ldd for these tests.
    optionalString (!stdenv.hostPlatform.isAarch32 && !stdenv.hostPlatform.isAarch64) (
      if stdenv.hostPlatform.isMusl then ''
        printf "checking that executable is statically linked... " >& 2
        (${haskellLib.lddForTests} $exe 2>&1 || true) | grep -i "not a"
      '' else ''
        printf "checking that executable is dynamically linked to system libraries... " >& 2
      '' + optionalString stdenv.isLinux ''
        ${haskellLib.lddForTests} $exe | grep 'libc\.so'
      '' + optionalString stdenv.isDarwin ''
        otool -L $exe |grep .dylib
    '') + ''

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isGhcjs && __elem compiler-nix-name ["ghc961" "ghc962"];
      disabled = broken;
    };

    passthru = {
      # Used for debugging with nix repl
      inherit packages;
    };
  };
}
