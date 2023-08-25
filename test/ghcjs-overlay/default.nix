{ stdenv, lib, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = cabalProject' {
    src = testSrc "ghcjs-overlay";
    inherit compiler-nix-name evalPackages;
    cabalProjectLocal = (if stdenv.hostPlatform.isGhcjs then ''
        repository ghcjs-overlay
          url: https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/bfc363b9f879c360e0a0460ec0c18ec87222ec32
          secure: True
          root-keys:
          key-threshold: 0
          --sha256: sha256-y1vQnXI1XzkjnC4h66tVDmu2TZjZPcMrZEnE3m0XOfg=
      ''
      else ''
        allow-newer: double-conversion:bytestring
      '')
      + lib.optionalString (__compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.8.0" >= 0) ''
      allow-newer: *:*

      repository head.hackage.ghc.haskell.org
        url: https://ghc.gitlab.haskell.org/head.hackage/
        secure: True
        key-threshold: 3
        root-keys:
          f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89
          26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329
          7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d
        --sha256: sha256-yMzVCP7DLb1Ztif1KCGk4RfREoROjtb6QBBtrSFy4OQ=

      active-repositories: hackage.haskell.org, head.hackage.ghc.haskell.org:override${lib.optionalString stdenv.hostPlatform.isGhcjs ", ghcjs-overlay:override"}
    '';
    # Alternative to the --sha256 comment in cabal.project
    # sha256map = {
    #  "https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/bfc363b9f879c360e0a0460ec0c18ec87222ec32" =
    #    "sha256-g9xGgJqYmiczjxjQ5JOiK5KUUps+9+nlNGI/0SpSOpg=";
    # };
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
    passthru = {
      inherit project;
    };
  };
}
