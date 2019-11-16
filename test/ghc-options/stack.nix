{ stdenv, stackProject' }:

with stdenv.lib;

let
  project = stackProject' {
    src = ./.;
  };
  packages = project.hsPkgs;
in
  stdenv.mkDerivation {
    name = "callStackToNix-test";

    buildCommand = ''
      printf "checking whether executable runs... " >& 2
      cat ${packages.test-ghc-options.components.exes.test-ghc-options-exe.run}

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  }
