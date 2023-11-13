{ stdenv, lib, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = cabalProject' {
    src = testSrc "ghcjs-overlay";
    inherit compiler-nix-name evalPackages;
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "ghcjs-overlay-test";

    buildCommand = ''
      exe="${packages.ghcjs-overlay-test.components.exes.ghcjs-overlay-test.exePath}"
      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2
      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.ghcjs-overlay-test.components.exes.ghcjs-overlay-test}/test-stdout
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
    meta.platforms = platforms.all;
    meta.disabled = haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64;
    passthru = {
      inherit project;
    };
  };
}
