with import ../. { nixpkgs = ../nixpkgs; nixpkgsArgs = { }; };
let Cabal = buildPackages.haskell-nix.hackage-package {
    name = "Cabal"; version = "2.4.1.0";
    modules = [
        { packages.Cabal.patches = [ ./Cabal-install-folder.diff ]; }
    ];
}; in
# (haskell-nix.stackProject {
#     src = ../cardano-wallet;
#     modules = [
#     	({config, ... }:{ packages.hello.package.setup-depends = [ Cabal ]; })
#     ];}).cardano-wallet.components.all
(let stack-pkgs = import ../cardano-sl/flags-test/pkgs.nix;
 in let pkg-set = haskell-nix.mkStackPkgSet
                { inherit stack-pkgs;
                  pkg-def-extras = [(hackage: {
                    packages = {
                       # "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
                        "process" = (((hackage.process)."1.6.5.0").revisions).default;
                    };
                  })
                  (hackage: {
                      packages = {
                        "hsc2hs" = (((hackage.hsc2hs)."0.68.4").revisions).default;
                    };
                  })];
                  modules = [
                              { packages.Cabal.patches = [ ./overlays/patches/Cabal/fix-data-dir.patch ]; }
                              { packages.alex.package.setup-depends = [pkg-set.config.hsPkgs.Cabal]; }
                              { packages.happy.package.setup-depends = [pkg-set.config.hsPkgs.Cabal]; }
                              { doHaddock = false; }
                            ];
                        #   ++ (args.modules or [])
                        #   ++ self.lib.optional (args ? ghc) { ghc.package = args.ghc; };
                };
            in pkg-set.config.hsPkgs
).cardano-sl.components.all
