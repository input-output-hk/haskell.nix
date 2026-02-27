# Test building with the reinstallable GHC library (stack)
{ stdenv, lib, haskell-nix, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  stackYaml = "stack-${compiler-nix-name}.yaml";

  project = haskell-nix.stackProject {
    inherit stackYaml;
    src = testSrc "ghc-lib-reinstallable";
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
    # Stack test is disabled for most compiler versions (only works with specific resolver)
    disabled =
      !(builtins.elem compiler-nix-name ["ghc9122" "ghc9123"])
      || stdenv.hostPlatform.isAndroid
      || stdenv.hostPlatform.isGhcjs
      || stdenv.hostPlatform.isWasm
      || stdenv.hostPlatform.isWindows;
  };

  ifdInputs = {
    inherit (project) stack-nix;
  };

  # Build the executable
  build = packages.ghc-lib-reinstallable.components.exes.ghc-lib-reinstallable;

  # Run test
  run = stdenv.mkDerivation {
    name = "ghc-lib-reinstallable-stack-test";

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

    meta.platforms = lib.platforms.unix;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
