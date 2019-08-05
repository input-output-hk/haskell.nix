self: super: rec {
    # Use this to disable the existing haskell infra structure for testing purposes
    haskell.compiler =
        let bootPkgs = with self.buildPackages; {
                ghc = buildPackages.bootstrap.haskell.compiler.ghc844;
                inherit (bootstrap.haskell.packages) alex happy hscolour;
            };
            sphinx = with self.buildPackages; (python3Packages.sphinx_1_7_9 or python3Packages.sphinx);
            hsc2hs-align-conditionals-patch = self.fetchpatch {
                url = "https://git.haskell.org/hsc2hs.git/patch/738f3666c878ee9e79c3d5e819ef8b3460288edf";
                sha256 = "0plzsbfaq6vb1023lsarrjglwgr9chld4q3m99rcfzx0yx5mibp3";
                extraPrefix = "utils/hsc2hs/";
                stripLen = 1;
            };
            D5123-patch = self.fetchpatch rec { # https://phabricator.haskell.org/D5123
                url = "http://tarballs.nixos.org/sha256/${sha256}";
                name = "D5123.diff";
                sha256 = "0nhqwdamf2y4gbwqxcgjxs0kqx23w9gv5kj0zv6450dq19rji82n";
            };
            haddock-900-patch = self.fetchpatch rec { # https://github.com/haskell/haddock/issues/900
                url = "https://patch-diff.githubusercontent.com/raw/haskell/haddock/pull/983.diff";
                name = "loadpluginsinmodules.diff";
                sha256 = "0bvvv0zsfq2581zsir97zfkggc1kkircbbajc2fz3b169ycpbha1";
                extraPrefix = "utils/haddock/";
                stripLen = 1;
            };
        in {
            ghc844 = self.callPackage ../compiler/ghc {
                inherit bootPkgs sphinx;

                buildLlvmPackages = self.buildPackages.llvmPackages_5;
                llvmPackages = self.llvmPackages_5;

                src-spec = rec {
                    version = "8.4.4";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "1ch4j2asg7pr52ai1hwzykxyj553wndg7wq93i47ql4fllspf48i";
                };

                ghc-patches = [ hsc2hs-align-conditionals-patch D5123-patch ]
                            ++ self.lib.optional self.stdenv.isDarwin ./patches/ghc/ghc-8.4.4-backport-dylib-command-size-limit.patch;
            };
            ghc861 = self.callPackage ../compiler/ghc {
                inherit bootPkgs sphinx;

                buildLlvmPackages = self.buildPackages.llvmPackages_5;
                llvmPackages = self.llvmPackages_5;

                src-spec = rec {
                    version = "8.6.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0dkh7idgrqr567fq94a0f5x3w0r4cm2ydn51nb5wfisw3rnw499c";
                };

                ghc-patches = [ D5123-patch ];
            };
            ghc862 = self.callPackage ../compiler/ghc {
                inherit bootPkgs sphinx;

                buildLlvmPackages = self.buildPackages.llvmPackages_5;
                llvmPackages = self.llvmPackages_5;

                src-spec = rec {
                    version = "8.6.2";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "1mbn3n2ynmpfpb7jfnhpzzli31qqxqyi8ws71blws3i846fq3ana";
                };

                ghc-patches = [ D5123-patch ];
            };
            ghc863 = self.callPackage ../compiler/ghc {
                inherit bootPkgs sphinx;

                buildLlvmPackages = self.buildPackages.llvmPackages_5;
                llvmPackages = self.llvmPackages_5;

                src-spec = rec {
                    version = "8.6.3";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "08vzq0dpg4a39bs61j6rq4z0n7jby5mc69h4m25xhd8rjyvkg7lz";
                };

                ghc-patches = [ D5123-patch ];
            };
            ghc864 = self.callPackage ../compiler/ghc {
                inherit bootPkgs sphinx;

                buildLlvmPackages = self.buildPackages.llvmPackages_5;
                llvmPackages = self.llvmPackages_5;

                src-spec = rec {
                    version = "8.6.4";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0fihs1sr0hpk67dn9cmrsav13kkcp9hz8ggdqcrs80rj8vj0fpav";
                };

                ghc-patches = [ D5123-patch ];
            };
            ghc865 = self.callPackage ../compiler/ghc {
                inherit bootPkgs sphinx;

                buildLlvmPackages = self.buildPackages.llvmPackages_5;
                llvmPackages = self.llvmPackages_5;

                src-spec = rec {
                    version = "8.6.5";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0qg3zsmbk4rkwkc3jpas3zs74qaxmw4sp4v1mhsbj0a0dzls2jjd";
                };

                ghc-patches = [ D5123-patch haddock-900-patch ];
            };
    };

    ghc = haskell.compiler.ghc865;
    cabal-install = self.buildPackages.bootstrap.haskell.packages.cabal-install;

    # see below
    haskellPackages.hpack = null;
    haskellPackages.hoogle = self.haskell-nix.haskellPackages.hoogle.components.exes.hoogle;

    # WARN: The `import ../. {}` will prevent
    #       any cross to work, as we will loose
    #       the `config` value.
    # As such the folloing sadly won't work :(
    # haskellPackages = with import ../. {}; {
    #     hpack = null;
    #     hello = (hackage-package {
    #         inherit (self) cabal-install;
    #         name = "hello"; version = "1.0.0.2";
    #     }).components.exes.hello;
    # };

    # stub out lib stuff.
    haskell.lib = {
        justStaticExecutables = x: x;
    };

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
        # XXX: import ../. will throw away all other overlays, config values, ...
        #      this is not ideal!
        haskell = with self.haskell-nix; let ghc = self.buildPackages.bootstrap.haskell.compiler.ghc844; in {
            # get binary compilers for bootstrapping.  We'll put the eventual proper
            # compilers into the same place where nix expects them.
            compiler = import ../compiler/old-ghc-nix { pkgs = self; };

            packages = {
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
                nix-tools = nix-tools.override {
                    inherit ghc;
                    inherit (bootstrap.haskell.packages) hpack;
                };

                # now that we have nix-tools and hpack, we can just
                # use `hackage-package` to build any package from
                # hackage with haskell.nix.  For alex and happy we
                # need to use the boot strap compiler as we need them
                # to build ghcs from source.
                alex = (hackage-package {
                    inherit ghc;
                    inherit (bootstrap.haskell.packages) cabal-install nix-tools hpack;
                    name = "alex"; version = "3.2.4";
                }).components.exes.alex;
                happy = (hackage-package {
                    inherit ghc;
                    inherit (bootstrap.haskell.packages) cabal-install nix-tools hpack;
                    name = "happy"; version = "1.19.11";
                }).components.exes.happy;

                hscolour = (hackage-package {
                    inherit ghc;
                    inherit (bootstrap.haskell.packages) cabal-install nix-tools hpack;
                    name = "hscolour"; version = "1.24.4";
                }).components.exes.HsColour;
            };
        };
    };
}