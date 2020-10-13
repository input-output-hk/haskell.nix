{ stdenv, mkCabalProjectPkgSet, cabalProject', haskellLib, util, recurseIntoAttrs, testSrc, compiler-nix-name }:

with stdenv.lib;

let
  project = cabalProject' {
    inherit compiler-nix-name;
    src = testSrc "cabal-22";
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  shell = util.addCabalInstall packages.project.components.library;
  run = stdenv.mkDerivation {
    name = "cabal-22-test";

    buildCommand = ''
      exe="${packages.project.components.exes.project}/bin/project${stdenv.hostPlatform.extensions.executable}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.project.components.exes.project}/test-stdout

    '' +
    # Aarch is statically linked and does not produce a .so file.
    # Musl is also statically linked, but it does make a .so file so we should check that still.
    optionalString (!stdenv.hostPlatform.isAarch32 && !stdenv.hostPlatform.isAarch64) (''
      printf "checking that executable is dynamically linked to system libraries... " >& 2
    '' + optionalString (stdenv.isLinux && !stdenv.hostPlatform.isMusl) ''
      ldd $exe | grep libgmp
    '' + optionalString stdenv.isDarwin ''
      otool -L $exe | grep "libSystem.B"
    '' + ''
      # fixme: posix-specific
      printf "checking that dynamic library is produced... " >& 2
    '' + optionalString stdenv.isLinux ''
      sofile=$(find "${packages.project.components.library}" | grep -e '\.so$')
    '' + optionalString stdenv.isDarwin ''
      sofile=$(find "${packages.project.components.library}" | grep -e '\.dylib$')
    '' + ''
      echo "$sofile"
    '' + optionalString (!stdenv.hostPlatform.isMusl) (''
      printf "checking that dynamic library is dynamically linked to prim... " >& 2
    '' + optionalString stdenv.isLinux ''
      ldd $sofile | grep libHSghc-prim
    '' + optionalString stdenv.isDarwin ''
      otool -L $sofile | grep libHSghc-prim
    '')) + ''
      touch $out

      printf "checking whether benchmark ran... " >& 2
      cat ${haskellLib.check packages.project.components.benchmarks.project-bench}/test-stdout

      printf "checking whether tests ran... " >& 2
      cat ${haskellLib.check packages.project.components.tests.unit}/test-stdout
    '';

    meta.platforms = platforms.all;
    passthru = {
      inherit project;
    };
  };
}
