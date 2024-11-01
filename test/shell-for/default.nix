{ stdenv, lib, haskellLib, cabal-install, mkCabalProjectPkgSet, recurseIntoAttrs, runCommand, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [{
      pkga = ./.plan.nix/pkga.nix;
      pkgb = ./.plan.nix/pkgb.nix;
    }];
    modules = [{
      inherit evalPackages;
    }];
  };

  env = pkgSet.config.hsPkgs.shellFor {
    # Shell will provide the dependencies of pkga and pkgb, but not
    # pkga and pkgb themselves.
    packages = ps: with ps; [ pkga pkgb ];
    # This adds cabal-install to the shell, which helps tests because
    # they use a nix-shell --pure. Normally you would BYO cabal-install.
    tools = {
      cabal.cabalProjectLocal = builtins.readFile ../cabal.project.local;
      hoogle.index-state = "2024-10-26T00:00:00Z";
    };
    exactDeps = true;
    # Avoid duplicate package issues when runghc looks for packages
    packageSetupDeps = false;
  };

  envPkga = pkgSet.config.hsPkgs.shellFor {
    # Shell will provide the dependencies of pkga
    packages = ps: with ps; [ pkga ];
    # This adds cabal-install to the shell, which helps tests because
    # they use a nix-shell --pure. Normally you would BYO cabal-install.
    tools = {
      cabal.cabalProjectLocal = builtins.readFile ../cabal.project.local;
      hoogle.index-state = "2024-10-26T00:00:00Z";
    };
    exactDeps = true;
    # Avoid duplicate package issues when runghc looks for packages
    packageSetupDeps = false;
  };

  envDefault = pkgSet.config.hsPkgs.shellFor {
    # The default implementation of packages should use isLocal and the
    # result should be the same as:
    #   packages = ps: with ps; [ pkga pkgb ];
    # This adds cabal-install to the shell, which helps tests because
    # they use a nix-shell --pure. Normally you would BYO cabal-install.
    tools = {
      cabal.cabalProjectLocal = builtins.readFile ../cabal.project.local;
      hoogle.index-state = "2024-10-26T00:00:00Z";
    };
    # Avoid duplicate package issues when runghc looks for packages
    packageSetupDeps = false;
  };

in recurseIntoAttrs {
  # Does not work on ghcjs because it needs zlib.
  # Does not work on windows because it needs mintty.
  meta.disabled = stdenv.hostPlatform.isMusl || stdenv.hostPlatform.isGhcjs || stdenv.hostPlatform.isWindows || (haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64)
    || compiler-nix-name != ((import ./pkgs.nix).pkgs null).compiler.nix-name;
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

    meta = {
      platforms = platforms.unix;
    };

    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet;

      # Used for testing externally with nix-shell (../tests.sh).
      inherit env envPkga envDefault;
    };
  };
}
