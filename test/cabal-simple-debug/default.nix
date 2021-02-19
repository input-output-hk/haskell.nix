# Test a package set
{ stdenv, lib, util, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, dwarfdump }:

with lib;

let
  project = cabalProject' {
    inherit compiler-nix-name;
    src = testSrc "cabal-simple-debug";
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
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
      # DWARF only works on linux with GHC 8.10.2 and newer
      disabled = compiler-nix-name == "ghc865" || compiler-nix-name == "ghc884"
        || !stdenv.hostPlatform.isLinux;
    };

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
