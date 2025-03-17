{ lib, stdenv, testSrc, haskell-nix, compiler-nix-name, evalPackages, recurseIntoAttrs, buildPackages }:
let
  project = haskell-nix.cabalProject' {
    inherit compiler-nix-name evalPackages;
    name = "haskell-language-server";
    src = haskell-nix.sources.hls;
    configureArgs = "--disable-benchmarks --disable-tests"; # This makes cabalProject' more like the `tool` function
    cabalProjectLocal = ''
      if impl(ghc >=9.6.7) && impl(ghc <9.7) || impl(ghc >=9.8.3)
        constraints: ghc-lib-parser >=9.8.4
        allow-older: ghc-lib-parser:filepath
    '';
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
