{ stdenv, cabalProject', recurseIntoAttrs, haskellLib, testSrc, compiler-nix-name }:

with stdenv.lib;

let
  project = cabalProject' {
    inherit compiler-nix-name;
    index-state = "2020-05-25T00:00:00Z";
    src = testSrc "ghc-options";
    # TODO find a way to get the ghc-options into plan.json so we can use it in plan-to-nix
    modules = [ { packages.test-ghc-options.package.ghcOptions = "-DTEST_GHC_OPTION"; } ];
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "call-cabal-project-to-nix-test";

    buildCommand = ''
      printf "checking whether executable runs... " >& 2
      cat ${haskellLib.check packages.test-ghc-options.components.exes.test-ghc-options-exe}/test-stdout

      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Attributes used for debugging with nix repl
      inherit project packages;
    };
  };
}
