{ testSrc, evalPackages, buildPackages, compiler-nix-name, recurseIntoAttrs }:
let
  inherit (buildPackages.haskell-nix.tool compiler-nix-name "haskell-language-server" { modules = [{ reinstallableLibGhc = true; }]; }) project;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";

  # Haskell Language Server in hackage does not build for GHC 9.2 yet
  meta.disabled = __elem compiler-nix-name ["ghc921" "ghc922" "ghc923"];
}
