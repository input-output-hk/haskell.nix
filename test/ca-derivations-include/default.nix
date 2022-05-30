# Build a project enabling content addressed derivations for
# only a subset of the components
{ stdenv, pkgs, lib, mkCabalProjectPkgSet, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, CADerivationsEnabled }:

with lib;

let

  modules = [
    {
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.cabal-simple.doHaddock = false;
    }
  ];

  cabalProject = ''
    packages: .
    allow-newer: aeson:*
  '';

  src = testSrc "cabal-simple";

  # each derivation is content addressed
  projectA = project' {
    inherit compiler-nix-name modules src cabalProject;
    contentAddressed = {
      enable = true;
    };
  };

  # each derivation but one (the executable) is content addressed
  projectB = project' {
    inherit compiler-nix-name modules src cabalProject;
    contentAddressed = {
      enable = true;
      include = name: name != "cabal-simple-exe-cabal-simple";
    };
  };

  exeA = projectA.hsPkgs.cabal-simple.components.exes.cabal-simple.exePath;
  exeB = projectB.hsPkgs.cabal-simple.components.exes.cabal-simple.exePath;

in
recurseIntoAttrs {

  meta.disabled = !CADerivationsEnabled;

  # check if the built executables are different (one is content addressed)
  # the other components are all content addressed (same output paths then)
  run = stdenv.mkDerivation {
    name = "ca-derivations-include-test";

    buildCommand = ''
      [ "${exeA}" == "${exeB}" ] && exit 1
      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit projectB projectA;
    };
  };

}
