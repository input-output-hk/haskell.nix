{ lib, stdenv, testSrc, haskell-nix, compiler-nix-name, evalPackages, recurseIntoAttrs, buildPackages }:
let
  project = haskell-nix.cabalProject' {
    inherit compiler-nix-name evalPackages;
    name = "haskell-language-server";
    src = haskell-nix.sources."hls-2.10";
    configureArgs = "--disable-benchmarks --disable-tests"; # This makes cabalProject' more like the `tool` function
  };
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";

  # hls does not need to be cross compiled.
  meta.disabled =
    stdenv.hostPlatform != stdenv.buildPlatform
    || __compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.0.1" < 0
    || __compareVersions buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.11.0" >= 0;
}
