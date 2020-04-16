{ stdenv, cabal-install, mkCabalProjectPkgSet, recurseIntoAttrs, runCommand, testSrc }:

with stdenv.lib;

let
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [{
      pkga = ./.plan.nix/pkga.nix;
      pkgb = ./.plan.nix/pkgb.nix;
    }];
    modules = [{
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.bytestring-builder.doHaddock = false;
    }];
  };

  env = pkgSet.config.hsPkgs.shellFor {
    # Shell will provide the dependencies of pkga and pkgb, but not
    # pkga and pkgb themselves.
    packages = ps: with ps; [ pkga pkgb ];
    # This adds cabal-install to the shell, which helps tests because
    # they use a nix-shell --pure. Normally you would BYO cabal-install.
    buildInputs = [ cabal-install ];
    exactDeps = true;
  };

  envPkga = pkgSet.config.hsPkgs.shellFor {
    # Shell will provide the dependencies of pkga
    packages = ps: with ps; [ pkga ];
    # This adds cabal-install to the shell, which helps tests because
    # they use a nix-shell --pure. Normally you would BYO cabal-install.
    buildInputs = [ cabal-install ];
    exactDeps = true;
  };

  envDefault = pkgSet.config.hsPkgs.shellFor {
    # The default implementation of packages should use isLocal and the
    # result should be the same as:
    #   packages = ps: with ps; [ pkga pkgb ];
    # This adds cabal-install to the shell, which helps tests because
    # they use a nix-shell --pure. Normally you would BYO cabal-install.
    buildInputs = [ cabal-install ];
  };

in recurseIntoAttrs (if stdenv.hostPlatform.isWindows
 then
    let skip = runCommand "skip-test-shell-for" {} ''
      echo "Skipping shell-for test on windows as does not work yet" >& 2
      touch $out
    '';
    in {
      env = skip;
      envPkga = skip;
      envDefault = skip;
      run = skip;
    }
 else {
  inherit env envPkga envDefault;
  run = stdenv.mkDerivation {
    name = "shell-for-test";

    buildCommand = ''
      ########################################################################
      # test shell-for with an example program

      cp ${./pkgb/src}/*.hs .

      printf "checking that the shell env has the dependencies...\n" >& 2
      ${env.ghc}/bin/${env.ghc.targetPrefix}runghc conduit-test.hs

      printf "checking that the shell envDefault has the dependencies...\n" >& 2
      ${envDefault.ghc}/bin/${env.ghc.targetPrefix}runghc conduit-test.hs

      touch $out
    '';

    meta.platforms = platforms.all;
    meta.disabled = stdenv.hostPlatform.isMusl || stdenv.hostPlatform.isWindows;

    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet;

      # Used for testing externally with nix-shell (../tests.sh).
      inherit env envPkga envDefault;
    };
  };
})
