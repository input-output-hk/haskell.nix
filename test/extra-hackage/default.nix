{ stdenv, lib, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let

  hackage = import ./hackage;

  tarballs = {
    extra-hackage-demo = ./01-index.tar.gz;
  };

  demo-src = ./external-package-demo-0.1.0.0.tar.gz;

  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "extra-hackage/external-package-user";

    extra-hackages = [ hackage ];
    extra-hackage-tarballs = tarballs;
    cabalProjectLocal = optionalString (__compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.8.0" >= 0) ''
      allow-newer: *:*

      repository head.hackage.ghc.haskell.org
        url: https://ghc.gitlab.haskell.org/head.hackage/
        secure: True
        key-threshold: 3
        root-keys:
          f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89
          26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329
          7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d
        --sha256: sha256-WrhyfhCN6IiP5/yCyQibeHHfUDoAUioAH2ysMLoRqdg=

      active-repositories: hackage.haskell.org, head.hackage.ghc.haskell.org:override, extra-hackage-demo:override
    '';

    modules = [
      # To prevent nix-build from trying to download it from the
      # actual Hackage.
      { packages.external-package-demo.src = demo-src; }
    ];
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "external-hackage-test";

    buildCommand = ''
      exe="${packages.external-package-user.components.exes.external-package-user.exePath}"
      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2
      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.external-package-user.components.exes.external-package-user}/test-stdout
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
      broken = stdenv.hostPlatform.isGhcjs && __compareVersion buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.6.1" >= 0;
      disabled = broken;
    };

    passthru = {
      inherit project;
    };
  };
}
