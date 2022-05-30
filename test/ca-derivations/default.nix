# Test if derivations are content addressed building two derivations producing
# the same outputs and checking if the path stores are equals
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

  srcPlain = testSrc "cabal-simple";

  # we alter the source adding an Haskell comment since they are ignored by ghc
  srcWithComment = pkgs.runCommand "src-with-comment" { } ''
    mkdir $out
    install ${srcPlain}/* $out
    echo " -- Altering source without altering executable..." >> $out/Main.hs
  '';

  projectPlain = project' {
    inherit compiler-nix-name modules cabalProject;
    src = srcPlain;
    contentAddressed.enable = true;
  };

  projectWithComment = project' {
    inherit compiler-nix-name modules cabalProject;
    src = srcWithComment;
    contentAddressed.enable = true;
  };

  exe-plain = projectPlain.hsPkgs.cabal-simple.components.exes.cabal-simple.exePath;
  exe-withComment = projectWithComment.hsPkgs.cabal-simple.components.exes.cabal-simple.exePath;

in
recurseIntoAttrs {

  meta.disabled = !CADerivationsEnabled;

  run = stdenv.mkDerivation {
    name = "ca-derivations-test";

    buildCommand = ''
      [ "${exe-plain}" != "${exe-withComment}" ] && exit 1
      touch $out
    '';

    meta.platforms = platforms.all;

    passthru = {
      # Used for debugging with nix repl
      inherit projectWithComment projectPlain;
    };
  };

}
