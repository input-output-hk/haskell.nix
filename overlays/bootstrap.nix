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
            ghc-patches = version: let
                # Returns true iff this derivation's version is strictly older than ver.
                versionOlder = ver: builtins.compareVersions ver version == 1;
                # Returns true iff this derivation's verion is greater than or equal to ver.
                versionAtLeast = ver: !versionOlder ver;
                # Patches for which we know they have been merged into a public release already
                in self.lib.optional (versionAtLeast "8.4.4" && versionOlder "8.6")   ./patches/ghc/ghc-8.4.4-reinstallable-lib-ghc.patch
                ++ self.lib.optional (versionOlder "8.6")                             ./patches/ghc/move-iserv-8.4.2.patch
                ++ self.lib.optional (versionOlder "8.6")                             ./patches/ghc/hsc2hs-8.4.2.patch
                ++ self.lib.optional (versionOlder "8.6")                             ./patches/ghc/various-8.4.2.patch
                ++ self.lib.optional (versionOlder "8.6")                             ./patches/ghc/lowercase-8.4.2.patch
                ++ self.lib.optional (versionOlder "8.6")                             ./patches/ghc/cabal-exe-ext-8.4.2.patch
                ++ self.lib.optional (versionOlder "8.6")                             ./patches/ghc/ghc-8.4.3-Cabal2201-SMP-test-fix.patch
                ++ self.lib.optional (versionOlder "8.6")                             ./patches/ghc/outputtable-assert-8.4.patch
                ++ self.lib.optional (versionAtLeast "8.6" && versionOlder "8.6.4")   ./patches/ghc/MR148--T16104-GhcPlugins.patch
                ++ self.lib.optional (versionOlder "8.6.4")                           ./patches/ghc/MR95--ghc-pkg-deadlock-fix.patch

                # Patches for which we only know a lower bound.
                ++ self.lib.optional (versionAtLeast "8.6")                           ./patches/ghc/iserv-proxy-cleanup.patch                             # https://gitlab.haskell.org/ghc/ghc/merge_requests/250  -- merged; ghc-8.8.1
                ++ self.lib.optional (versionAtLeast "8.2")                           ./patches/ghc/MR545--ghc-pkg-databases.patch                        # https://gitlab.haskell.org/ghc/ghc/merge_requests/545  -- merged; ghc-8.8.1
                ++ self.lib.optional (versionAtLeast "8.6")                           ./patches/ghc/outputtable-assert-8.6.patch
                ++ self.lib.optional (versionAtLeast "8.6")                           ./patches/ghc/mistuke-ghc-err_clean_up_error_handler-8ab1a89af89848f1713e6849f189de66c0ed7898.diff # this is part of Phyx- revamped io-manager.
                ++ self.lib.optional (versionAtLeast "8.6.4")                         ./patches/ghc/ghc-8.6.4-reenable-th-qq-in-stage1.patch
                ++ [
                ./patches/ghc/ghc-add-keepCAFs-to-rts.patch                         # https://gitlab.haskell.org/ghc/ghc/merge_requests/950  -- open
                ./patches/ghc/lowercase-8.6.patch                                   # https://gitlab.haskell.org/ghc/ghc/merge_requests/949  -- merged; ghc-8.8.1
                ./patches/ghc/dll-loader-8.4.2.patch                                # https://gitlab.haskell.org/ghc/ghc/merge_requests/949  -- open
                ./patches/ghc/0001-Stop-the-linker-panic.patch                      # https://phabricator.haskell.org/D5012                  -- merged; ghc-8.8.1
                ./patches/ghc/ghc-8.4.3-Cabal2201-no-hackage-tests.patch            # ?
                ./patches/ghc/ghc-8.4.3-Cabal2201-allow-test-wrapper.patch          # https://github.com/haskell/cabal/pulls/5995            -- merged; cabal-3.0.0 (ghc-8.8.1)
                ./patches/ghc/ghc-8.4.3-Cabal2201-response-file-support.patch       # https://github.com/haskell/cabal/pulls/5996            -- merged; cabal-3.0.0 (ghc-8.8.1)
                ./patches/ghc/ghc-8.6-Cabal-fix-datadir.patch                       # https://github.com/haskell/cabal/issues/5862
                ./patches/ghc/MR196--ghc-pkg-shut-up.patch                          # https://gitlab.haskell.org/ghc/ghc/merge_requests/196  -- merged; ghc-8.8.1
                ./patches/ghc/MR948--32bit-cross-th.patch                           # https://gitlab.haskell.org/ghc/ghc/merge_requests/948  -- open
                ]

                # Patches for specific ghc versions.
                ++ self.lib.optional (version == "8.6.3")                             ./patches/ghc/T16057--ghci-doa-on-windows.patch
                ++ self.lib.optional (version == "8.6.3")                             ./patches/ghc/ghc-8.6.3-reinstallable-lib-ghc.patch
                ++ self.lib.optional (version == "8.6.4")                             ./patches/ghc/ghc-8.6.4-reinstallable-lib-ghc.patch
                ++ self.lib.optional (version == "8.6.5")                             ./patches/ghc/ghc-8.6.5-reinstallable-lib-ghc.patch
                ++ self.lib.optional (version == "8.6.4")                             ./patches/ghc/ghc-8.6.4-better-plusSimplCountErrors.patch
                ++ self.lib.optional (versionAtLeast "8.6.4" && self.stdenv.isDarwin) ./patches/ghc/ghc-macOS-loadArchive-fix.patch
                ;
        in ({
            ghc844 = self.callPackage ../compiler/ghc {
                inherit bootPkgs sphinx;

                buildLlvmPackages = self.buildPackages.llvmPackages_5;
                llvmPackages = self.llvmPackages_5;

                src-spec = rec {
                    version = "8.4.4";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "1ch4j2asg7pr52ai1hwzykxyj553wndg7wq93i47ql4fllspf48i";
                };

                ghc-patches = ghc-patches "8.4.4"
                            ++ [ hsc2hs-align-conditionals-patch D5123-patch ]
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

                ghc-patches = ghc-patches "8.6.2"
                            ++ [ D5123-patch ];
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

                ghc-patches = ghc-patches "8.6.3"
                            ++ [ D5123-patch ];
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

                ghc-patches = ghc-patches "8.6.4"
                            ++ [ D5123-patch ];
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

                ghc-patches = ghc-patches "8.6.5"
                            ++ [ D5123-patch haddock-900-patch ];
            };
        } // self.lib.optionalAttrs (self.targetPlatform.isGhcjs or false) {
            ghc865 = let ghcjs865 = self.callPackage ../compiler/ghcjs/ghcjs.nix {
                ghcjsSrcJson = ../compiler/ghcjs/ghcjs-src.json;
                ghcjsVersion =  "8.6.0.1";
                ghc = self.buildPackages.haskell.compiler.ghc865;
            }; in let targetPrefix = "js-unknown-ghcjs-"; in self.runCommand "${targetPrefix}ghc-8.6.5" {
                passthru = {
                    inherit targetPrefix;
                    version = "8.6.5";
                };
            } ''
                mkdir -p $out/bin
                cd $out/bin
                ln -s ${ghcjs865}/bin/ghcjs ${targetPrefix}ghc
                ln -s ${ghcjs865}/bin/ghcjs-pkg ${targetPrefix}ghc-pkg
                ln -s ${ghcjs865}/bin/hsc2hs-ghcjs ${targetPrefix}hsc2hs
                cd ..
                mkdir lib
                cd lib
                cp -R ${ghcjs865}/lib/ghcjs-8.6.5 ${targetPrefix}ghc-8.6.5
                '';
        });

    ghc = haskell.compiler.ghc865;
    cabal-install = self.buildPackages.bootstrap.haskell.packages.cabal-install;

    # see below
    haskellPackages.hpack = null;
    haskellPackages.hoogle = self.haskell-nix.haskellPackages.hoogle.components.exes.hoogle;
    haskellPackages.happy = self.haskell-nix.haskellPackages.happy.components.exes.happy;
    haskellPackages.alex = self.haskell-nix.haskellPackages.alex.components.exes.alex;

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
