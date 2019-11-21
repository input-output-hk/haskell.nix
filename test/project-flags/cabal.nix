{ stdenv, cabalProject', recurseIntoAttrs }:

with stdenv.lib;

let
  project = cabalProject' {
    name = "test-project-flags";
    index-state = "2019-04-30T00:00:00Z";
    src = ./.;
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  inherit (project) plan-nix;
  run = stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      exe="${packages.test-project-flags.components.exes.test-project-flags-exe}/bin/test-project-flags-exe${stdenv.hostPlatform.extensions.executable}"

      printf "checking whether executable runs... " >& 2
      cat ${packages.test-project-flags.components.exes.test-project-flags-exe.run}

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
      plan-nix = plan.nix;
    };
  };
}
