# Test a package set
{ stdenv, lib, util, mkCabalProjectPkgSet, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name }:

with lib;

let
  modules = [
     {
       # Package has no exposed modules which causes
       #   haddock: No input file(s)
       packages.cabal-simple.doHaddock = false;
     }
  ];

  project = project' {
    inherit compiler-nix-name;
    src = testSrc "cabal-simple";
    inherit modules;
    cabalProject = ''
      packages: .
      allow-newer: aeson:*
    '';
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  # Used for testing externally with nix-shell (../tests.sh).
  test-shell = project.shellFor { tools = { cabal = "3.6.2.0"; }; withHoogle = !__elem compiler-nix-name ["ghc901" "ghc902" "ghc921" "ghc922"]; };

  run = stdenv.mkDerivation {
    name = "cabal-simple-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple.exePath}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.cabal-simple.components.exes.cabal-simple}/test-stdout
    '' + (if stdenv.hostPlatform.isMusl
      then ''
        printf "checking that executable is statically linked... " >& 2
        (ldd $exe 2>&1 || true) | grep -i "not a"
      ''
      else
        # Skip this on aarch as we do not have an `ldd` tool
        optionalString (!stdenv.hostPlatform.isAarch32 && !stdenv.hostPlatform.isAarch64) (''
          printf "checking that executable is dynamically linked to system libraries... " >& 2
        '' + optionalString stdenv.isLinux ''
          ldd $exe | grep libpthread
        '' + optionalString stdenv.isDarwin ''
          otool -L $exe |grep .dylib
      '')) + ''

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
