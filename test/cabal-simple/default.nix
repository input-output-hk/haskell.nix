# Test a package set
{ stdenv, util, mkCabalProjectPkgSet, cabalProject', haskellLib, gmp6, zlib, recurseIntoAttrs }:

with stdenv.lib;

let
  modules = [
     {
       # Package has no exposed modules which causes
       #   haddock: No input file(s)
       packages.cabal-simple.doHaddock = false;

       # When compiling with musl ghc, build a statically linked
       # executable against static libraries.
       # Ref: https://vaibhavsagar.com/blog/2018/01/03/static-haskell-nix/
       packages.cabal-simple.components.exes.cabal-simple.configureFlags =
         optionals stdenv.hostPlatform.isMusl [
           "--disable-executable-dynamic"
           "--disable-shared"
           "--ghc-option=-optl=-pthread"
           "--ghc-option=-optl=-static"
           "--ghc-option=-optl=-L${gmp6.override { withStatic = true; }}/lib"
           # "--ghc-option=-optl=-L${zlib.static}/lib"
         ];
     }
  ];

  project = cabalProject' {
    name = "cabal-simple";
    src = haskellLib.cleanGit { src = ../..; subDir = "test/cabal-simple"; };
    inherit modules;
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  inherit (project) plan-nix;
  run = stdenv.mkDerivation {
    name = "cabal-simple-test";

    buildCommand = ''
      exe="${packages.cabal-simple.components.exes.cabal-simple}/bin/cabal-simple${stdenv.hostPlatform.extensions.executable}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${packages.cabal-simple.components.exes.cabal-simple.run}
    '' + (if stdenv.hostPlatform.isMusl then ''
        printf "checking that executable is statically linked... " >& 2
        (ldd $exe 2>&1 || true) | grep -i "not a"
      '' else ''
        printf "checking that executable is dynamically linked to system libraries... " >& 2
      '' + optionalString stdenv.isLinux ''
        ldd $exe | grep libpthread
      '' + optionalString stdenv.isDarwin ''
        otool -L $exe |grep .dylib
    '') + ''

      printf "Checking that \"all\" component has the programs... " >& 2
      all_exe="${packages.cabal-simple.components.all}/bin/cabal-simple${stdenv.hostPlatform.extensions.executable}"
      test -f "$all_exe"
      echo "$all_exe" >& 2

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;

      # Used for testing externally with nix-shell (../tests.sh).
      # This just adds cabal-install to the existing shells.
      test-shell = util.addCabalInstall packages.cabal-simple.components.all;
    };
  };
}
