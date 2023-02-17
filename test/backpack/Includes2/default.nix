{ stdenv, lib, cabalProject', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "backpack/Includes2";
  };

  packages = project.hsPkgs;

in recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.Includes2.components.exes.exe;
  check = haskellLib.check build;
}
