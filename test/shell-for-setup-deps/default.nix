{ stdenv, cabal-install, cabalProject', recurseIntoAttrs, runCommand, testSrc, compiler-nix-name }:

with stdenv.lib;

let
  project = cabalProject' {
    inherit compiler-nix-name;
    src = testSrc "shell-for-setup-deps";
    modules = [{
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.bytestring-builder.doHaddock = false;
    }];
  };

  env = project.shellFor {};

in recurseIntoAttrs ({
  ifdInputs = {
    inherit (project) plan-nix;
  };
  inherit env;
  run = stdenv.mkDerivation {
    name = "shell-for-setup-deps-test";

    buildCommand = ''
      ########################################################################
      # test shell-for with an example program

      cp ${./pkg/src}/*.hs .

      printf "checking that the shell env has the dependencies...\n" >& 2
      ${env.ghc}/bin/${env.ghc.targetPrefix}ghc-pkg list
      ${env.ghc}/bin/${env.ghc.targetPrefix}runghc conduit-test.hs

      touch $out
    '';

    meta = {
      platforms = platforms.all;
      # Making this work for cross compilers will be difficult as setup-deps are
      # built for the build platform and the shell will be for the host platform.
      # We probably need a shell that provides both build and host ghc
      # and corresponding package DBs and a way to use them.
      # This problem affects musl as well as the build libraries are linked to glibc.
      disabled = stdenv.buildPlatform != stdenv.hostPlatform;
    };

    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet;

      # Used for testing externally with nix-shell (../tests.sh).
      inherit project env;
    };
  };
})
