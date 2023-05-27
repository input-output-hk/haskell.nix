# Test a package set
{ stdenv, lib, util, mkCabalProjectPkgSet, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

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
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-simple";
    inherit modules;
    cabalProject = ''
      packages: .
      allow-newer: aeson:*
    '' + lib.optionalString (__elem compiler-nix-name ["ghc96020230302" "ghc961" "ghc962"]) ''
      allow-newer: *:base, *:ghc-prim, *:template-haskell
    '';
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  # Used for testing externally with nix-shell (../tests.sh).
  test-shell = (project.shellFor {
      tools = { cabal = "latest"; };
      withHoogle = !__elem compiler-nix-name ["ghc901" "ghc902" "ghc921" "ghc922" "ghc923" "ghc924" "ghc925" "ghc926" "ghc927"];
    }).overrideAttrs (_: _: {
      meta = rec {
        platforms = lib.platforms.all;
        broken = stdenv.hostPlatform.isGhcjs && __elem compiler-nix-name ["ghc961" "ghc962"];
        disabled = broken;
      };
    });

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
        (${haskellLib.lddForTests} $exe 2>&1 || true) | grep -i "not a"
      ''
      else
        # Skip this on aarch as we do not have an `ldd` tool
        optionalString (!stdenv.hostPlatform.isAarch32 && !stdenv.hostPlatform.isAarch64) (''
          printf "checking that executable is dynamically linked to system libraries... " >& 2
        '' + optionalString stdenv.isLinux ''
          ${haskellLib.lddForTests} $exe | grep 'libc\.so'
        '' + optionalString stdenv.isDarwin ''
          otool -L $exe |grep .dylib
      '')) + ''

      touch $out
    '';

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isGhcjs && __elem compiler-nix-name ["ghc961" "ghc962"];
      disabled = broken;
    };

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
