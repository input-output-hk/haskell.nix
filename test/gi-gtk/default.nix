# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "gi-gtk";
    cabalProjectLocal = builtins.readFile ../cabal.project.local + ''
      -- haskell-gi 0.26.12 breaks gi-gtkpixbuf
      index-state: 2024-09-30T00:00:00Z
      -- The overloading feature of haskell-gi makes build times very long
      constraints: haskell-gi-overloading ==0.0
      if impl(ghc >=9.11)
        constraints: filepath source
    '';
  };

  packages = project.hsPkgs;

in recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs
    # Gtk cross compilation seems to be broken in nixpkgs
    || stdenv.hostPlatform.isWindows
    # We can't make static libraries for Gtk
    || stdenv.hostPlatform.isMusl
    # Older versions of GHC fail for aarch64 with
    # error: incompatible pointer to integer conversion assigning to 'ffi_arg' (aka 'unsigned long') from 'HsPtr' (aka 'void *') [-Wint-conversion]
    || builtins.elem compiler-nix-name ["ghc8107" "ghc902" "ghc928" "ghc948"] && stdenv.hostPlatform.isAarch64
    # Cross compilation to aarch64 is also broken
    || stdenv.hostPlatform.isAarch64 && !stdenv.buildPlatform.isAarch64
    # glu is marked ase broken for isAndroid
    || stdenv.hostPlatform.isAndroid
    # Building profiled version of Cabal for haskell-gi is currently broken for GHC head
    || compiler-nix-name == "ghc91320241204";

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.test-gi-gtk.components.exes.test-gi-gtk;
  check = haskellLib.check build;
  build-profiled = packages.test-gi-gtk.components.exes.test-gi-gtk.profiled;
  check-profiled = haskellLib.check build-profiled;
}
