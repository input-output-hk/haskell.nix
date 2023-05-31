{ stdenv, lib, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = cabalProject' {
    src = testSrc "ghcjs-overlay";
    inherit compiler-nix-name evalPackages;
    cabalProjectLocal = if stdenv.hostPlatform.isGhcjs then ''
        repository ghcjs-overlay
          url: https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/bfc363b9f879c360e0a0460ec0c18ec87222ec32
          secure: True
          root-keys:
          key-threshold: 0
          --sha256: sha256-y1vQnXI1XzkjnC4h66tVDmu2TZjZPcMrZEnE3m0XOfg=
      ''
      else ''
        allow-newer: double-conversion:bytestring
      '';
    # Alternative to the --sha256 comment in cabal.project
    # sha256map = {
    #  "https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/bfc363b9f879c360e0a0460ec0c18ec87222ec32" =
    #    "sha256-g9xGgJqYmiczjxjQ5JOiK5KUUps+9+nlNGI/0SpSOpg=";
    # };
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  meta.disabled = __elem compiler-nix-name ["ghc941" "ghc942" "ghc943" "ghc944" "ghc945" "ghc96020230302" "ghc961" "ghc962"];
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
    passthru = {
      inherit project;
    };
  };
}
