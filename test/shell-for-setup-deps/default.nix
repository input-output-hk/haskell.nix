{ stdenv, cabal-install, cabalProject', recurseIntoAttrs, runCommand }:

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

in recurseIntoAttrs (if stdenv.hostPlatform.isWindows
 then
    let skip = runCommand "skipping-test-shell-for-setup-deps" {} ''
      echo "Skipping shell-for-setup-deps test on windows as does not work yet" >& 2
      touch $out
    '';
    in {
      ifdInputs = recurseIntoAttrs { plan-nix = skip; };
      env = skip;
      run = skip;
    }
 else {
  ifdInputs = recurseIntoAttrs {
    inherit (project) plan-nix;
  };
  inherit env;
  run = stdenv.mkDerivation {
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
    meta.disabled = stdenv.hostPlatform.isMusl || stdenv.hostPlatform.isWindows;

    passthru = {
      # Used for debugging with nix repl
      inherit pkgSet;

      # Used for testing externally with nix-shell (../tests.sh).
      inherit project env;
    };
  };
})
