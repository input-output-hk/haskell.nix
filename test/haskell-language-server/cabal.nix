{ stdenv, testSrc, haskell-nix, compiler-nix-name, evalPackages, recurseIntoAttrs }:
let
  inherit (haskell-nix.tool compiler-nix-name "haskell-language-server" {
    version =
      if __compareVersions haskell-nix.compiler.${compiler-nix-name}.version "9.0" < 0
        then "1.8.0.0"
        else "latest";
    inherit evalPackages; }) project;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";

  # hls does not need to be cross compiled.
  meta.disabled = stdenv.hostPlatform != stdenv.buildPlatform || __elem compiler-nix-name ["ghc941" "ghc942" "ghc943" "ghc944"];
}
