self: super: rec {
    # Use this to disable the existing haskell infra structure for testing purposes
#    haskell.compiler = {};

    # NOTE: 8.6.5 prebuilt binaries on macOS, will yield:
    # > Linking dist/build/cabal/cabal ...
    # > Undefined symbols for architecture x86_64:
    # >  "_utimensat", referenced from:
    # >      _cazW_info in libHSdirectory-1.3.3.0.a(Posix.o)
    # > ld: symbol(s) not found for architecture x86_64
    # > clang-5.0: error: linker command failed with exit code 1 (use -v to see invocation)
    # > `clang' failed in phase `Linker'. (Exit code: 1)


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
            happy = (hackage-package {
                ghc = bootstrap.haskell.compiler.ghc844;
                inherit (bootstrap.haskell) cabal-install;
                name = "happy"; version = "1.19.11";
            }).components.exes.happy;
        };
    };
}