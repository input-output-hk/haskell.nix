self: super: rec {
    # Use this to disable the existing haskell infra structure for testing purposes
    # haskell.compiler = {};

    # NOTE: 8.6.5 prebuilt binaries on macOS, will yield:
    #
    # > Linking dist/build/cabal/cabal ...
    # > Undefined symbols for architecture x86_64:
    # >  "_utimensat", referenced from:
    # >      _cazW_info in libHSdirectory-1.3.3.0.a(Posix.o)
    # > ld: symbol(s) not found for architecture x86_64
    # > clang-5.0: error: linker command failed with exit code 1 (use -v to see invocation)
    # > `clang' failed in phase `Linker'. (Exit code: 1)
    #
    # hence we'll use 844 for bootstrapping for now.


    # the bootstrap infra structure (pre-compiled ghc; bootstrapped cabal-install, ...)
    bootstrap = {
        haskell = with import ../. {}; let ghc = bootstrap.haskell.compiler.ghc844; in {
            # get binary compilers for bootstrapping.  We'll put the eventual proper
            # compilers into the same place where nix expects them.
            compiler = import ../compiler/old-ghc-nix { pkgs = self; };

            # cabal has it's own bootstrap script which we'll use.
            cabal-install = import ../compiler/bootstrap/cabal-install.nix {
                inherit (self) fetchurl stdenv zlib;
                inherit hackage ghc;
                src = self.fetchurl {
                    url = "https://github.com/haskell/cabal/archive/Cabal-v3.0.0.0-rc3.tar.gz";
                    sha256 = "1zl2mgg8307ykq3v8nmafc6zdhhj1cw7w8ffpap16dsm65lbnx33";
                };
                version = "3.0.0.0";
            };

            # disable hpack support during bootstrap
            hpack = null;
            nix-tools = (callPackage ../nix-tools {
                inherit ghc;
                inherit (bootstrap.haskell) hpack;
            });

            # now that we have nix-tools and hpack, we can just
            # use `hackage-package` to build any package from
            # hackage with haskell.nix.  For alex and happy we
            # need to use the boot strap compiler as we need them
            # to build ghcs from source.
            alex = (hackage-package {
                inherit ghc;
                inherit (bootstrap.haskell) cabal-install nix-tools hpack;
                name = "alex"; version = "3.2.4";
            }).components.exes.alex;
            happy = (hackage-package {
                inherit ghc;
                inherit (bootstrap.haskell) cabal-install nix-tools hpack;
                name = "happy"; version = "1.19.11";
            }).components.exes.happy;
        };
    };
}