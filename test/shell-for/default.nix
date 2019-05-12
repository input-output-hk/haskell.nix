{ stdenv, util, mkStackSnapshotPkgSet }:

with stdenv.lib;
with util;

let
  pkgSet = mkStackSnapshotPkgSet {
    resolver = "lts-12.21";
    # Work around a mismatch between stackage metadata and the
    # libraries shipped with GHC.
    # https://github.com/commercialhaskell/stackage/issues/4466
    pkg-def-extras = [(hackage: {
      packages = {
        "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
        "process" = (((hackage.process)."1.6.5.0").revisions).default;
      };
    })];
  };

  env = pkgSet.config.hsPkgs.shellFor {
    packages = ps: with ps; [ conduit conduit-extra resourcet ];
    withHoogle = true;
  };

in
  stdenv.mkDerivation {
    name = "shell-for-test";

    buildCommand = ''
      ########################################################################
      # test shell-for with an example program

      printf "checking that the shell env has the dependencies...\n" >& 2
      ${env.ghc}/bin/runghc ${./conduit.hs}

      touch $out
    '';

    meta.platforms = platforms.all;
    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet;

      # Used for testing externally with nix-shell (../tests.sh).
      # This just adds cabal-install to the existing shell.
      env = addCabalInstall env;
    };
}
