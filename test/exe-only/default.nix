# Test a package set
{ stdenv, util, haskell-nix, recurseIntoAttrs, haskellLib }:

with stdenv.lib;

let
  project = haskell-nix.cabalProject' {
    src = ./.;
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "exe-only-test";

    buildCommand = ''
      exe="${packages.exe-only.components.exes.exe-only}/bin/exe-only${stdenv.hostPlatform.extensions.executable}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable ran... " >& 2
      cat ${haskellLib.check packages.exe-only.components.exes.exe-only}
    '' +
    # Aarch are statically linked and does not have ldd for these tests.
    optionalString (!stdenv.hostPlatform.isAarch32 && !stdenv.hostPlatform.isAarch64) (
      if stdenv.hostPlatform.isMusl then ''
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
      all_exe="${packages.exe-only.components.all}/bin/exe-only${stdenv.hostPlatform.extensions.executable}"
      test -f "$all_exe"
      echo "$all_exe" >& 2

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet packages;

      # Used for testing externally with nix-shell (../tests.sh).
      # This just adds cabal-install to the existing shells.
      test-shell = util.addCabalInstall packages.exe-only.components.all;
    };
  };
}
