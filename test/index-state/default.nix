# Test a package set
{ stdenv, lib, testSrc, tool, compiler-nix-name, evalPackages }:

with lib;

let
  # The hackage-security 0.6.0.1 was uploaded at 2020-04-06T20:54:35Z
  # See https://hackage.haskell.org/package/hackage-security-0.6.0.1
  version-used-at = index-state: ((tool compiler-nix-name "cabal" {
      version = "3.2.0.0";
      inherit index-state evalPackages;
      cabalProject = ''
        packages: .
        allow-newer: cabal-install:base, hackage-security:*
        package cabal-install
          flags: -native-dns
      '';
    }).project.getPackage "hackage-security").components.library.version;
  version-before = version-used-at "2020-04-06T20:54:34Z";
  version-after = version-used-at "2020-04-06T20:54:35Z";

in
  stdenv.mkDerivation {
    name = "index-state-test";

    buildCommand = ''
      if [[ "${version-before}" != "0.6.0.0" ]]; then
        echo 'Unexpected version ${version-before} (expected "0.6.0.0")'
        exit 1
      fi
      if [[ "${version-after}" != "0.6.0.1" ]]; then
        echo 'Unexpected version ${version-after} (expected "0.6.0.1")'
        exit 1
      fi

      touch $out
    '';

    meta.platforms = platforms.all;
    # This test will need to be updated to use newer hackage index-state for it
    # to work with GHC 9 and above.
    # Does not work for GHCJS
    # We can probably enable ghc961 again once Cabal 3.10 is in hackage
    meta.disabled = stdenv.hostPlatform.isGhcjs || __elem compiler-nix-name ["ghc901" "ghc902" "ghc921" "ghc922" "ghc923" "ghc924" "ghc925" "ghc926" "ghc927" "ghc928" "ghc961" "ghc962"];
    

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  }
