# Test backpack
{ stdenv, lib, haskellLib, cabalProject', testSrc, compiler-nix-name, evalPackages }:

let
  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "backpack";
    inherit (evalPackages.haskell-nix) nix-tools;
    cabalProjectLocal = builtins.readFile ../cabal.project.local
      + lib.optionalString (haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64) ''
        constraints: text -simdutf, text source
    '';
  };
in lib.recurseIntoAttrs {
  ifdInputs = { inherit (project) plan-nix; };
  build = project.hsPkgs.backpack.components.exes.backpack;
}
