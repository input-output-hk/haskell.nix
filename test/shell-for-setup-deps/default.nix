{ stdenv, cabal-install, cabalProject', recurseIntoAttrs }:

with stdenv.lib;

let
  project = cabalProject' {
    name = "test-shell-for-setup-deps";
    src = ./.;
    modules = [{
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.bytestring-builder.doHaddock = false;
    }];
  };

  env = project.hsPkgs.shellFor {};

in recurseIntoAttrs {
  inherit (project) plan-nix;
  inherit env;
  run = stdenv.mkDerivation {
    name = "shell-for-test";

    buildCommand = optionalString (!stdenv.hostPlatform.isWindows) ''
      ########################################################################
      # test shell-for with an example program

      cp ${./pkg/src}/*.hs .

      printf "checking that the shell env has the dependencies...\n" >& 2
      ${env.ghc}/bin/runghc conduit-test.hs

      touch $out
    '';

    meta.platforms = platforms.all;
    meta.disabled = stdenv.hostPlatform.isMusl || stdenv.hostPlatform.isWindows;

    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet;

      # Used for testing externally with nix-shell (../tests.sh).
      inherit project env;
    };
  };
}
