{ stdenv, util, mkPkgSet, recurseIntoAttrs, testSrc }:

with stdenv.lib;
with util;

let
  pkgs = import ./pkgs.nix;
  pkgSet = mkPkgSet {
    # generated with:
    #   cabal new-build
    #   plan-to-nix -o .
    pkg-def = pkgs.pkgs;
    pkg-def-extras = [ pkgs.extras ];
    modules = [
      # overrides to fix the build
      {
        packages.transformers-compat.components.library.doExactConfig = true;
      }

      # vary component config for tests
      {
        packages.test-with-packages.components = {
          all.doExactConfig = mkForce false; # the default; use force because of quirk in all. See package.nix
          library.doExactConfig = true; # not the default
        };
      }
    ];
  };

  packages = pkgSet.config.hsPkgs;

  package = packages.test-with-packages;
  inherit (package.components) library;

  pkgId = p: "${p.identifier.name}-${p.identifier.version}";
  showDepends = component: concatMapStringsSep " " pkgId component.depends;

in recurseIntoAttrs {
  # Used for testing externally with nix-shell (../tests.sh).
  # This just adds cabal-install to the existing shells.
  test-shell = addCabalInstall packages.test-with-packages.components.all;

  # A variant of test-shell with the component option doExactConfig enabled
  test-shell-dec = addCabalInstall packages.test-with-packages.components.library;

  run = stdenv.mkDerivation {
    name = "with-packages-test";
    libraryDepends = showDepends pkgSet.config.packages.test-with-packages.components.library;
    allDepends = showDepends pkgSet.config.packages.test-with-packages.components.all;

    buildCommand = ''
      ########################################################################
      # test with-packages

      printf "checking merging of the 'all' component depends ... " >& 2
      if [ -n "$libraryDepends" -a "$libraryDepends" = "$allDepends" ]; then
        echo "PASS" >& 2
      else
        echo "FAIL" >& 2
        echo "libraryDepends = $libraryDepends"
        echo "allDepends = $allDepends"
        exit 1
      fi

      printf "checking that the 'all' component works... " >& 2
      echo ${package.components.all} >& 2

      printf "checking that the 'library' component works... " >& 2
      echo ${package.components.library} >& 2
    '' + (if stdenv.hostPlatform.isWindows
      then ''
        printf "runghc tests are not working yet for windows. skipping. " >& 2
      ''
      else ''
        printf "checking that the package env has the dependencies... " >& 2
        ${package.components.all.env}/bin/${package.components.all.env.targetPrefix}runghc ${./Point.hs}
        echo >& 2

        printf "checking that components.library.env has the dependencies... " >& 2
        ${library.env}/bin/${library.env.targetPrefix}runghc ${./Point.hs}
        echo >& 2
      '') + ''
      touch $out
    '';

    meta.platforms = platforms.all;
    meta.disabled = stdenv.hostPlatform.isMusl;

    passthru = {
      # Used for debugging with nix repl
      inherit packages pkgSet;
    };
  };
}
