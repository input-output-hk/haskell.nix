# Test building with the reinstallable GHC library (cabal)
{ stdenv, lib, haskell-nix, haskellLib, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  ghcVersion = buildPackages.haskell-nix.compiler.${compiler-nix-name}.version;

  project = haskell-nix.cabalProject {
    inherit compiler-nix-name evalPackages;
    src = testSrc "ghc-lib-reinstallable";
    cabalProjectLocal = ''
      constraints: ghc ==${ghcVersion}
    '';
    modules = [{
      # Configure packages for os-string
      packages.directory.components.library.configureFlags = ["-f os-string"];
      packages.file-io.components.library.configureFlags = ["-f os-string"];
      packages.unix.components.library.configureFlags = ["-f os-string"];
      packages.process.components.library.configureFlags = ["-f os-string"];
    }];
  };

  packages = project.hsPkgs;

in lib.recurseIntoAttrs {
  meta = {
    # Only run this test for GHC 9.12+ where the fix is needed
    disabled =
      (builtins.compareVersions ghcVersion "9.12" < 0)
      || stdenv.hostPlatform.isGhcjs
      || stdenv.hostPlatform.isWindows;  # Skip on Windows for now
  };

  ifdInputs = {
    inherit (project) plan-nix;
  };

  # Build the executable
  build = packages.ghc-lib-reinstallable.components.exes.ghc-lib-reinstallable;

  # Run test
  run = stdenv.mkDerivation {
    name = "ghc-lib-reinstallable-cabal-test";

    buildCommand = ''
      exe="${packages.ghc-lib-reinstallable.components.exes.ghc-lib-reinstallable.exePath}"

      printf "checking whether executable was built... " >& 2
      test -f "$exe" && echo "OK" >& 2

      size=$(command stat --format '%s' "$exe" 2>/dev/null || stat -f '%z' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # Check that it runs (skip on cross-compilation)
      ${optionalString (!haskellLib.isCrossHost) ''
        printf "checking whether executable runs... " >& 2
        cat ${haskellLib.check packages.ghc-lib-reinstallable.components.exes.ghc-lib-reinstallable}/test-stdout
        echo "OK" >& 2
      ''}

      printf "Successfully built executable depending on GHC library\n" >& 2
      touch $out
    '';

    meta = {
      platforms = lib.platforms.unix;
      # Disable for GHC versions before 9.12 and for problematic platforms
      disabled =
        (builtins.compareVersions ghcVersion "9.12" < 0)
        || stdenv.hostPlatform.isGhcjs
        || stdenv.hostPlatform.isWindows;
    };

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
