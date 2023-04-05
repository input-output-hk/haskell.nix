{ stdenv, testSrc, haskell-nix, compiler-nix-name, evalPackages, recurseIntoAttrs }:
let
  inherit (haskell-nix.tool compiler-nix-name "haskell-language-server" {
    version = "github-1.10";
    inherit evalPackages; }) project;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";

  # hls does not need to be cross compiled.
  meta.disabled = stdenv.hostPlatform != stdenv.buildPlatform || __elem compiler-nix-name ["ghc941" "ghc942" "ghc943" "ghc944" "ghc96020230302" "ghc961"];
}
