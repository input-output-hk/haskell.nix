{ testSrc, evalPackages, buildPackages, compiler-nix-name, recurseIntoAttrs }:
let
  inherit (buildPackages.haskell-nix.tool compiler-nix-name "haskell-language-server" "latest") project;
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.getComponent "haskell-language-server:exe:haskell-language-server";
}
