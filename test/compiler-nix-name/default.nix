{ stdenv, lib, haskell-nix, recurseIntoAttrs, testSrc }:

with lib;

let
  project = haskell-nix.cabalProject' {
    src = testSrc "compiler-nix-name";
    compiler-nix-name = "ghc884";
  };

  packages = project.hsPkgs;
  compiler-nix-name =
    packages.compiler-nix-name.components.exes.compiler-nix-name;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  run = stdenv.mkDerivation {
    name = "compiler-nix-name-test";

    buildCommand = ''
      exe="${compiler-nix-name.exePath}"
      ver="$(${toString compiler-nix-name.config.testWrapper} $exe)"
      if [[ "$ver" != "808" ]]; then
        echo "Unexpected GHC version (expected 808, but got $ver)" >& 2
        false
      else
        touch $out
      fi
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit project packages;
    };
  };
}
