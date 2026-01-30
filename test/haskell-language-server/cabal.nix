{ lib, stdenv, testSrc, haskell-nix, compiler-nix-name, evalPackages, buildPackages }:
let
  project = haskell-nix.tool compiler-nix-name "haskell-language-server" {
    inherit evalPackages;
  };
in lib.recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";

  # hls does not need to be cross compiled.
  meta.disabled =
    stdenv.hostPlatform != stdenv.buildPlatform
    || __compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.0.1" < 0
    || __compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.13" >= 0;
}
