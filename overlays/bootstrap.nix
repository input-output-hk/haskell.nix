self: super: rec {
    # Use this to disable the existing haskell infra structure for testing purposes
#    haskell.compiler = {};

    # the bootstrap infra structure (pre-compiled ghc; bootstrapped cabal-install, ...)
    bootstrap = {
        haskell = with import ../. {}; {
            compiler = import ../compiler/old-ghc-nix { pkgs = self; };
            cabal-install = import ../compiler/bootstrap/cabal-install.nix {
                inherit (self) fetchurl stdenv zlib;
                inherit hackage;
                ghc = bootstrap.haskell.compiler.ghc844;
                src = self.fetchurl {
                    url = "https://github.com/haskell/cabal/archive/Cabal-v3.0.0.0-rc3.tar.gz";
                    sha256 = "1zl2mgg8307ykq3v8nmafc6zdhhj1cw7w8ffpap16dsm65lbnx33";
                };
                version = "3.0.0.0";
            };
            alex = (hackage-package {
                ghc = bootstrap.haskell.compiler.ghc844;
                inherit (bootstrap.haskell) cabal-install;
                name = "alex"; version = "3.2.4";
            }).components.exes.alex;
            happy = (haskell-package {
                ghc = bootstrap.haskell.compiler.ghc844;
                inherit (bootstrap.haskell) cabal-install;
                name = "happy"; version = "1.19.11";
            }).components.exes.happy;
        };
    };
}