with import ../. { nixpkgs = ../../nixpkgs; nixpkgsArgs = { }; };
let Cabal = buildPackages.haskell-nix.hackage-package {
    name = "Cabal"; version = "2.4.1.0";
    modules = [
        { packages.Cabal.patches = [ ./Cabal-install-folder.diff ]; }
    ];
}; in
(haskell-nix.stackProject {
    src = ../../cardano-wallet;
    pkg-def-extras = [(hackage: {
      packages = {
          "hsc2hs" = (((hackage.hsc2hs)."0.68.4").revisions).default;
        };
      })];
    modules = [
    	({config, ... }:{ packages.hello.package.setup-depends = [ Cabal ]; })
    ];}).cardano-wallet.components.all
