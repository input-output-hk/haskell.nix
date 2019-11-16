{ stdenv, cabalProject' }:

with stdenv.lib;

let
  project = cabalProject' {
    name = "test-ghc-options";
    index-state = "2019-04-30T00:00:00Z";
    src = ./.;
    # TODO find a way to get the ghc-options into plan.json so we can use it in plan-to-nix
    modules = [ { packages.test-ghc-options.package.ghcOptions = "-DTEST_GHC_OPTION"; } ];
  };
  packages = project.hsPkgs;
in
  stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

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
