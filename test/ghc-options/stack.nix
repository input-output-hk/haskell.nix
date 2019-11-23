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
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.test-ghc-options.components.exes.test-ghc-options-exe}

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
}