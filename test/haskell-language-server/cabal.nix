{ testSrc, evalPackages, buildPackages, compiler-nix-name, recurseIntoAttrs }:
let
  inherit (buildPackages.haskell-nix.tool compiler-nix-name "haskell-language-server" "latest") project;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";

  # Haskell Language Server does not build for GHC 9 yet
  meta.disabled = __elem compiler-nix-name ["ghc901" "ghc902" "ghc921" "ghc922"];
}
