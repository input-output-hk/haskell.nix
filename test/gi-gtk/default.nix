# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "gi-gtk";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
  };

  packages = project.hsPkgs;

in recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs
    # Gtk cross compilation seems to be broken in nixpkgs
    || stdenv.hostPlatform.isWindows;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.test-gi-gtk.components.exes.test-gi-gtk;
  check = haskellLib.check build;
  build-profiled = packages.test-gi-gtk.components.exes.test-gi-gtk.profiled;
  check-profiled = haskellLib.check build-profiled;
}
