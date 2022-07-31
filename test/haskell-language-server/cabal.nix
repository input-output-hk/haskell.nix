{ stdenv, testSrc, haskell-nix, compiler-nix-name, evalPackages, recurseIntoAttrs }:
let
  inherit (haskell-nix.tool compiler-nix-name "haskell-language-server" { inherit evalPackages; }) project;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";

  # Haskell Language Server in hackage does not build for GHC 9.2 yet
  meta.disabled = __elem compiler-nix-name ["ghc921" "ghc922" "ghc923" "ghc924"] || stdenv.hostPlatform != stdenv.buildPlatform;
}
