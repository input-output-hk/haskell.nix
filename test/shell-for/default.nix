{ stdenv, lib, haskellLib, testSrc, compiler-nix-name, evalPackages, project' }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "shell-for";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = [{ inherit evalPackages; }];
  };

  packages = project.hsPkgs;

  env = project.shellFor {
    packages = ps: with ps; [ pkga pkgb ];
    tools = {
      cabal.cabalProjectLocal = builtins.readFile ../cabal.project.local;
      hoogle.cabalProjectLocal = builtins.readFile ../cabal.project.local;
    };
    exactDeps = true;
    packageSetupDeps = false;
  };

  envPkga = project.shellFor {
    # Shell will provide the dependencies of pkga
    packages = ps: with ps; [ pkga ];
    # This adds cabal-install to the shell, which helps tests because
    # they use a nix-shell --pure. Normally you would BYO cabal-install.
    tools = {
      cabal.cabalProjectLocal = builtins.readFile ../cabal.project.local;
      hoogle.cabalProjectLocal = builtins.readFile ../cabal.project.local;
    };
    exactDeps = true;
    # Avoid duplicate package issues when runghc looks for packages
    packageSetupDeps = false;
  };

  envDefault = project.shellFor {
    # The default implementation of packages should use isLocal and the
    # result should be the same as:
    #   packages = ps: with ps; [ pkga pkgb ];
    # This adds cabal-install to the shell, which helps tests because
    # they use a nix-shell --pure. Normally you would BYO cabal-install.
    tools = {
      cabal.cabalProjectLocal = builtins.readFile ../cabal.project.local;
      hoogle.cabalProjectLocal = builtins.readFile ../cabal.project.local;
    };
    # Avoid duplicate package issues when runghc looks for packages
    packageSetupDeps = false;
  };

in lib.recurseIntoAttrs {
  # Does not work on ghcjs because it needs zlib.
  # Does not work on windows because it needs mintty.
  meta.disabled = stdenv.hostPlatform.isMusl
    || stdenv.hostPlatform.isGhcjs
    || stdenv.hostPlatform.isWasm
    || stdenv.hostPlatform.isWindows
    || (haskellLib.isCrossHost && (stdenv.hostPlatform.isAarch64 || stdenv.hostPlatform.isAarch32));
  inherit env envPkga envDefault;
  run = stdenv.mkDerivation {
    name = "shell-for-test";

    buildCommand = ''
      ########################################################################
      # test shell-for with an example program

      cp ${testSrc "shell-for" + "/pkgb/src"}/*.hs .

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
      inherit project packages;

      # Used for testing externally with nix-shell (../tests.sh).
      inherit env envPkga envDefault;
    };
  };
}
