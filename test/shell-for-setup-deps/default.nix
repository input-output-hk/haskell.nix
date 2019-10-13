{ stdenv, cabal-install, mkCabalProjectPkgSet, callCabalProjectToNix, importAndFilterProject }:

with stdenv.lib;

let
  plan = importAndFilterProject (callCabalProjectToNix {
    src = ./.;
  });
  pkgSet = mkCabalProjectPkgSet {
    plan-pkgs = plan.pkgs;
    modules = [{
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.bytestring-builder.doHaddock = false;
    }];
  };

  env = pkgSet.config.hsPkgs.shellFor {};

 in
  stdenv.mkDerivation {
    name = "shell-for-test";

    buildCommand = ''
      ########################################################################
      # test shell-for with an example program

      cp ${./pkg/src}/*.hs .

      printf "checking that the shell env has the dependencies...\n" >& 2
      ${env.ghc}/bin/runghc conduit-test.hs

      touch $out
    '';

    meta.platforms = platforms.all;
    meta.disabled = stdenv.hostPlatform.isMusl;

    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet;

      # Used for testing externally with nix-shell (../tests.sh).
      inherit env;
      plan-nix = plan.nix;
    };
}
