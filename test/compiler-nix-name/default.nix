{ stdenv, haskell-nix, recurseIntoAttrs, testSrc }:

with stdenv.lib;

let
  project = haskell-nix.cabalProject' {
    src = testSrc "compiler-nix-name";
    compiler-nix-name = "ghc883";
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
      exe="${compiler-nix-name}/bin/compiler-nix-name${stdenv.hostPlatform.extensions.executable}"
      if [[ "$(${toString compiler-nix-name.config.testWrapper} $exe)" != "808" ]]; then
        echo "Unexpected GHC version" >& 2
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
