{ stdenv, stackProject', recurseIntoAttrs, haskellLib }:

with stdenv.lib;

let
  project = stackProject' {
    src = ./.;
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = recurseIntoAttrs {
    inherit (project) stack-nix;
  };
  run = stdenv.mkDerivation {
    name = "callStackToNix-test";

    buildCommand = ''
      exe="${packages.test-project-flags.components.exes.test-project-flags-exe}/bin/test-project-flags-exe${stdenv.hostPlatform.extensions.executable}"

      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.test-project-flags.components.exes.test-project-flags-exe}

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
      stack-nix = stack.nix;
    };
  };
}
