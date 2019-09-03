with import ./. { nixpkgs = ../nixpkgs; nixpkgsArgs = { crossSystem = { config = "armv6l-unknown-linux-gnueabihf"; }; }; };
let Cabal = buildPackages.haskell-nix.hackage-package {
    name = "Cabal"; version = "2.4.1.0";
    modules = [
        { packages.Cabal.patches = [ ./Cabal-install-folder.diff ]; }
    ];
}; in
(haskell-nix.hackage-package {
    name = "hello"; version = "1.0.0.2";
    modules = [
    	({config, ... }:{ packages.hello.package.setup-depends = [ Cabal ]; })
    ];}).components.exes.hello