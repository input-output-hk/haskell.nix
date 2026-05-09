# Test a package set
{ stdenv, lib, util, cabalProject', haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "sublib-docs";
    cabalProjectLocal = builtins.readFile ../cabal.project.local + ''

      package *
        documentation: True
    '';
  };

  packages = project.hsPkgs;

in lib.recurseIntoAttrs {
  # Haddock is not included with cross compilers currently
  meta.disabled = haskellLib.isCrossHost || stdenv.hostPlatform.isStatic;
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "sublib-docs-test";

    buildCommand = ''
      exe="${packages.sublib-docs.components.exes.sublib-docs.exePath}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.sublib-docs.components.exes.sublib-docs}/test-stdout

    '' +
    # Musl and Aarch are statically linked..
    optionalString (!stdenv.hostPlatform.isAarch32 && !stdenv.hostPlatform.isAarch64 && !stdenv.hostPlatform.isMusl) (''
      printf "checking that executable is dynamically linked to system libraries... " >& 2
    '' + optionalString (stdenv.isLinux && !stdenv.hostPlatform.isMusl) ''
      ${haskellLib.lddForTests} $exe | grep 'libc[.]so'
    '' + optionalString stdenv.isDarwin ''
      otool -L $exe |grep .dylib
    '') + ''

      printf "check that it looks like we have docs..." >& 2
      # cabal v2-haddock writes docs into each unit's
      # `share/doc/html/`; the unit-id is cabal-mangled-and-hashed,
      # so we locate the html via `find` rather than a synthesised
      # path.
      test -f "$(find ${packages.sublib-docs.components.library.doc} \
                       -path '*/share/doc/html/Lib.html' \
                       -print -quit)"
      test -f "$(find ${packages.sublib-docs.components.sublibs.slib.doc} \
                       -path '*/share/doc/html/*Slib.html' \
                       -print -quit)"

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit packages;
    };
  };
}
