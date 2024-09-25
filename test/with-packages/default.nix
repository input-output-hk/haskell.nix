{ stdenv, lib, haskellLib, util, cabalProject', recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;
with util;

let
  project = doExactConfig: cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "with-packages";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = [
      # overrides to fix the build
      {
        packages.transformers-compat.components.library.doExactConfig = true;
      }

      # vary component config for tests
      {
        packages.test-with-packages.components.library.doExactConfig = doExactConfig;
      }

      { inherit evalPackages; }
    ];
  };

  packages = doExactConfig: (project doExactConfig).hsPkgs;

  package = doExactConfig: (packages doExactConfig).test-with-packages;

  decLibrary = (package true).components.library;
  library = (package false).components.library;

  pkgId = p: "${p.identifier.name}-${p.identifier.version}";
  showDepends = component: concatMapStringsSep " " pkgId (component.depends or []);
  extraFlags = "";

in recurseIntoAttrs {
  # Used for testing externally with nix-shell (../tests.sh).
  # This just adds cabal-install to the existing shells.
  test-shell = (addCabalInstall library.shell).overrideAttrs (_: _: {
    meta = {
      platforms = lib.platforms.all;
    };
  });

  # A variant of test-shell with the component option doExactConfig enabled
  test-shell-dec = (addCabalInstall decLibrary.shell).overrideAttrs (_: _: {
    meta = {
      platforms = lib.platforms.all;
    };
  });

  run = stdenv.mkDerivation {
    name = "with-packages-test";
    decLibraryDepends = showDepends decLibrary.config;
    libraryDepends = showDepends library.config;

    src = ./.;

    buildPhase = ''
      ########################################################################
      # test with-packages

      printf "checking component depends ... " >& 2
      if [ -n "$decLibraryDepends" -a "$decLibraryDepends" = "$libraryDepends" ]; then
        echo "PASS" >& 2
      else
        echo "FAIL" >& 2
        echo "decLibraryDepends = $decLibraryDepends"
        echo "libraryDepends = $libraryDepends"
        exit 1
      fi

      printf "checking that the 'library' without doExactConfig works... " >& 2
      echo ${library} >& 2

      printf "checking that the 'library' with doExactConfig works... " >& 2
      echo ${decLibrary} >& 2
    '' + (if haskellLib.isCrossHost
      then ''
        printf "runghc tests are not working yet for windows or ghcjs. skipping. " >& 2
      ''
      else ''
        echo "checking that non doExactConfig library.env has the dependencies... "
        echo "with runghc"
        ${library.env}/bin/${library.env.targetPrefix}runghc ./Point.hs
        echo "with ghc"
        ${library.env}/bin/${library.env.targetPrefix}ghc ${toString extraFlags} Point.hs 1> /dev/null
        ./Point

        echo "checking that doExactConfig library.env has the dependencies... "
        echo "with runghc"
        ${decLibrary.env}/bin/${decLibrary.env.targetPrefix}runghc ./Point.hs
        echo "with ghc"
        ${decLibrary.env}/bin/${decLibrary.env.targetPrefix}ghc ${extraFlags} Point.hs 1> /dev/null
        ./Point
      '') + ''
      touch $out
    '';

    dontInstall = true;

    meta = rec {
      platforms = lib.platforms.all;
      broken = stdenv.hostPlatform.isMusl;
      disabled = broken;
    };

    passthru = {
      # Used for debugging with nix repl
      packages = packages false;
      project =  project false;
      packages-exact = packages true;
      project-exact =  project true;
    };
  };
}
