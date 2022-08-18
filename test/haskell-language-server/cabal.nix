{ stdenv, testSrc, haskell-nix, compiler-nix-name, evalPackages, recurseIntoAttrs }:
let
  inherit (haskell-nix.tool compiler-nix-name "haskell-language-server" { inherit evalPackages; }) project;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";

  # hls does not need to be cross compiled.
  meta.disabled = stdenv.hostPlatform != stdenv.buildPlatform;
}
