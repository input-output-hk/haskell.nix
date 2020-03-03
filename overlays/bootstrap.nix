self: super:
let
    installDeps = targetPrefix: ''
      for P in $($out/bin/${targetPrefix}ghc-pkg list --simple-output | sed 's/-[0-9][0-9.]*//g'); do
        mkdir -p $out/exactDeps/$P
        touch $out/exactDeps/$P/configure-flags
        touch $out/exactDeps/$P/cabal.config

        if id=$($out/bin/${targetPrefix}ghc-pkg field $P id --simple-output); then
          echo "--dependency=$P=$id" >> $out/exactDeps/$P/configure-flags
        elif id=$($out/bin/${targetPrefix}ghc-pkg field "z-$P-z-*" id --simple-output); then
          name=$($out/bin/${targetPrefix}ghc-pkg field "z-$P-z-*" name --simple-output)
          # so we are dealing with a sublib. As we build sublibs separately, the above
          # query should be safe.
          echo "--dependency=''${name#z-$P-z-}=$id" >> $out/exactDeps/$P/configure-flags
        fi
        if ver=$($out/bin/${targetPrefix}ghc-pkg field $P version --simple-output); then
          echo "constraint: $P == $ver" >> $out/exactDeps/$P/cabal.config
          echo "constraint: $P installed" >> $out/exactDeps/$P/cabal.config
        fi
      done

      mkdir -p $out/evalDeps
      for P in $($out/bin/${targetPrefix}ghc-pkg list --simple-output | sed 's/-[0-9.]*//g'); do
        touch $out/evalDeps/$P
        if id=$($out/bin/${targetPrefix}ghc-pkg field $P id --simple-output); then
          echo "package-id $id" >> $out/evalDeps/$P
        fi
      done
    '';
in {
  haskell-nix = super.haskell-nix // {
    # Use this to disable the existing haskell infra structure for testing purposes
    compiler =
        let bootPkgs = with self.buildPackages; {
                ghc = buildPackages.haskell-nix.bootstrap.compiler.ghc844;
                inherit (self.haskell-nix.bootstrap.packages) alex happy hscolour;
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
                versionLessThan = ver: builtins.compareVersions ver version == 1;
                # Returns true iff this derivation's verion is greater than or equal to ver.
                versionAtLeast = ver: !versionLessThan ver;
                from = start: self.lib.optional (versionAtLeast start);
                fromUntil = start: end: self.lib.optional (versionAtLeast start && versionLessThan end);
                until = end: self.lib.optional (versionLessThan end);
                always = self.lib.optional true;
                # Try to avoid reordering the patches unless a patch is added or changed that
                # will be applied to most versions of the GHC anyway (reordering the patches
                # results in rebuilds of GHC and reduces sharing in /nix/store).
                in fromUntil "8.4.4" "8.6"   ./patches/ghc/ghc-8.4.4-reinstallable-lib-ghc.patch
                ++ until             "8.6"   ./patches/ghc/iserv-move-8.4.1.patch                                # 6fbe5f274ba84181f5db50901639ae382ef68c4b               -- merged; ghc-8.6.1
                ++ until             "8.6"   ./patches/ghc/hsc2hs-8.4.2.patch
                ++ until             "8.6"   ./patches/ghc/various-8.4.2.patch
                ++ until             "8.6"   ./patches/ghc/lowercase-8.4.2.patch
                ++ until             "8.6"   ./patches/ghc/cabal-exe-ext-8.4.2.patch
                ++ until             "8.6"   ./patches/ghc/ghc-8.4.3-Cabal2201-SMP-test-fix.patch
                ++ until             "8.6"   ./patches/ghc/outputtable-assert-8.4.patch
                ++ fromUntil "8.6"   "8.6.4" ./patches/ghc/MR148--T16104-GhcPlugins.patch
                ++ until             "8.6.4" ./patches/ghc/MR95--ghc-pkg-deadlock-fix.patch
                ++ fromUntil "8.4"   "8.6"   ./patches/ghc/iserv-autoconf-8.4.1.patch                            # (same as below, but based on 8.4)
                ++ fromUntil "8.6"   "8.8"   ./patches/ghc/iserv-autoconf-8.6.1.patch                            # 8f9f52d8e421ce544d5437a93117545d52d0eabd               -- merged; ghc-8.8.1
                ++ fromUntil "8.4"   "8.6"   ./patches/ghc/iserv-cleanup-8.8.1-prepare-8.4.1.patch               # (prepare for below; see patch for details)
                ++ until             "8.10"  ./patches/ghc/iserv-cleanup-8.8.1.patch                             # https://gitlab.haskell.org/ghc/ghc/merge_requests/250  -- merged; ghc-8.10.1
                ++ fromUntil "8.4"   "8.6"   ./patches/ghc/iserv-cleanup-8.8.1-revert-8.4.1.patch                # (revert prepare)
                ++ fromUntil "8.2"   "8.8"   ./patches/ghc/MR545--ghc-pkg-databases.patch                        # https://gitlab.haskell.org/ghc/ghc/merge_requests/545  -- merged; ghc-8.8.1
                ++ fromUntil "8.6"   "8.8"   ./patches/ghc/outputtable-assert-8.6.patch
                ++ fromUntil "8.6.4" "8.8"   ./patches/ghc/ghc-8.6.4-reenable-th-qq-in-stage1.patch
                ++ until             "8.8"   ./patches/ghc/0001-Stop-the-linker-panic.patch                      # https://phabricator.haskell.org/D5012                  -- merged; ghc-8.8.1
                ++ until             "8.8"   ./patches/ghc/ghc-8.4.3-Cabal2201-allow-test-wrapper.patch          # https://github.com/haskell/cabal/pulls/5995            -- merged; cabal-3.0.0 (ghc-8.8.1)
                ++ until             "8.8"   ./patches/ghc/ghc-8.4.3-Cabal2201-response-file-support.patch       # https://github.com/haskell/cabal/pulls/5996            -- merged; cabal-3.0.0 (ghc-8.8.1)
                ++ until             "8.8"   ./patches/ghc/ghc-8.6-Cabal-fix-datadir.patch                       # https://github.com/haskell/cabal/issues/5862
                ++ until             "8.8"   ./patches/ghc/MR196--ghc-pkg-shut-up.patch                          # https://gitlab.haskell.org/ghc/ghc/merge_requests/196  -- merged; ghc-8.8.1
                ++ from      "8.6"           ./patches/ghc/mistuke-ghc-err_clean_up_error_handler-8ab1a89af89848f1713e6849f189de66c0ed7898.diff # this is part of Phyx- revamped io-manager.
                ++ always                    ./patches/ghc/ghc-add-keepCAFs-to-rts.patch                         # https://gitlab.haskell.org/ghc/ghc/merge_requests/950  -- open
                ++ always                    ./patches/ghc/lowercase-8.6.patch                                   # https://gitlab.haskell.org/ghc/ghc/merge_requests/949  -- merged; ghc-8.8.1
                ++ always                    ./patches/ghc/dll-loader-8.4.2.patch                                # https://gitlab.haskell.org/ghc/ghc/merge_requests/949  -- open
                ++ always                    ./patches/ghc/ghc-8.4.3-Cabal2201-no-hackage-tests.patch            # ?
                ++ always                    ./patches/ghc/MR948--32bit-cross-th.patch                           # https://gitlab.haskell.org/ghc/ghc/merge_requests/948  -- open
                ++ from      "8.8"           ./patches/ghc/cabal-host.patch                                      # https://github.com/haskell/cabal/issues/5887
                ++ fromUntil "8.6.4" "8.8"   ./patches/ghc/ghc-8.6.4-prim-no-arm-atomics.patch
                ++ fromUntil "8.6.4" "8.8"   ./patches/ghc/global-offset-table.patch
                ++ fromUntil "8.6.4" "8.8"   ./patches/ghc/global-offset-table-2.patch

                # Patches for specific ghc versions.
                ++ self.lib.optional (version == "8.6.3") ./patches/ghc/T16057--ghci-doa-on-windows.patch
                ++ self.lib.optional (version == "8.6.3") ./patches/ghc/ghc-8.6.3-reinstallable-lib-ghc.patch
                ++ self.lib.optional (version == "8.6.4") ./patches/ghc/ghc-8.6.4-reinstallable-lib-ghc.patch
                ++ self.lib.optional (version == "8.6.5") ./patches/ghc/ghc-8.6.5-reinstallable-lib-ghc.patch
                ++ self.lib.optional (version == "8.8.1") ./patches/ghc/ghc-8.8.1-reinstallable-lib-ghc.patch
                ++ self.lib.optional (version == "8.8.2") ./patches/ghc/ghc-8.8.2-reinstallable-lib-ghc.patch
                ++ self.lib.optional (version == "8.6.4") ./patches/ghc/ghc-8.6.4-better-plusSimplCountErrors.patch
                ++ self.lib.optional (versionAtLeast "8.6.4" && self.stdenv.isDarwin) ./patches/ghc/ghc-macOS-loadArchive-fix.patch
                ++ self.lib.optional (versionAtLeast "8.4.4" && self.stdenv.isDarwin) ./patches/ghc/ghc-darwin-gcc-version-fix.patch
                ;
        in ({
            ghc844 = self.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = self.buildPackages.haskell-nix.compiler.ghc844; };

                inherit bootPkgs sphinx installDeps;

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
                extra-passthru = { buildGHC = self.buildPackages.haskell-nix.compiler.ghc861; };

                inherit bootPkgs sphinx installDeps;

                buildLlvmPackages = self.buildPackages.llvmPackages_6;
                llvmPackages = self.llvmPackages_6;

                src-spec = rec {
                    version = "8.6.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0dkh7idgrqr567fq94a0f5x3w0r4cm2ydn51nb5wfisw3rnw499c";
                };

                ghc-patches = [ D5123-patch ];
            };
            ghc862 = self.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = self.buildPackages.haskell-nix.compiler.ghc862; };

                inherit bootPkgs sphinx installDeps;

                buildLlvmPackages = self.buildPackages.llvmPackages_6;
                llvmPackages = self.llvmPackages_6;

                src-spec = rec {
                    version = "8.6.2";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "1mbn3n2ynmpfpb7jfnhpzzli31qqxqyi8ws71blws3i846fq3ana";
                };

                ghc-patches = ghc-patches "8.6.2"
                            ++ [ D5123-patch ];
            };
            ghc863 = self.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = self.buildPackages.haskell-nix.compiler.ghc863; };

                inherit bootPkgs sphinx installDeps;

                buildLlvmPackages = self.buildPackages.llvmPackages_6;
                llvmPackages = self.llvmPackages_6;

                src-spec = rec {
                    version = "8.6.3";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "08vzq0dpg4a39bs61j6rq4z0n7jby5mc69h4m25xhd8rjyvkg7lz";
                };

                ghc-patches = ghc-patches "8.6.3"
                            ++ [ D5123-patch ];
            };
            ghc864 = self.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = self.buildPackages.haskell-nix.compiler.ghc864; };

                inherit bootPkgs sphinx installDeps;

                buildLlvmPackages = self.buildPackages.llvmPackages_6;
                llvmPackages = self.llvmPackages_6;

                src-spec = rec {
                    version = "8.6.4";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0fihs1sr0hpk67dn9cmrsav13kkcp9hz8ggdqcrs80rj8vj0fpav";
                };

                ghc-patches = ghc-patches "8.6.4"
                            ++ [ D5123-patch ];
            };
            ghc865 = self.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = self.buildPackages.haskell-nix.compiler.ghc865; };

                inherit bootPkgs sphinx installDeps;

                buildLlvmPackages = self.buildPackages.llvmPackages_6;
                llvmPackages = self.llvmPackages_6;

                src-spec = rec {
                    version = "8.6.5";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0qg3zsmbk4rkwkc3jpas3zs74qaxmw4sp4v1mhsbj0a0dzls2jjd";
                };

                ghc-patches = ghc-patches "8.6.5"
                            ++ [ D5123-patch haddock-900-patch ];
            };
            ghc881 = self.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = self.buildPackages.haskell-nix.compiler.ghc881; };

                inherit bootPkgs sphinx installDeps;

                buildLlvmPackages = self.buildPackages.llvmPackages_7;
                llvmPackages = self.llvmPackages_7;

                src-spec = rec {
                    version = "8.8.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "06kj4fhvijinjafiy4s873n60qly323rdlz9bmc79nhlp3cq72lh";
                };

                ghc-patches = ghc-patches "8.8.1";
            };
            ghc882 = self.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = self.buildPackages.haskell-nix.compiler.ghc882; };

                inherit bootPkgs sphinx installDeps;

                buildLlvmPackages = self.buildPackages.llvmPackages_7;
                llvmPackages = self.llvmPackages_7;

                src-spec = rec {
                    version = "8.8.2";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "02qa6wgjpxgakg7hv4zfdlrx9k7zxa5i02wnr6y9fsv8j16sbkh1";
                };

                ghc-patches = ghc-patches "8.8.2";
            };
        } // self.lib.optionalAttrs (self.targetPlatform.isGhcjs or false)
                # This will inject `exactDeps` and `envDeps`  into the ghcjs
                # compiler defined below.  This is crucial to build packages
                # with the current use of env and exact Deps.
                (builtins.mapAttrs
                    (_: v: v // {
                        isHaskellNixBootCompiler = true;
                    })
          ({
            ghc865 = let buildGHC = self.buildPackages.haskell-nix.compiler.ghc865;
                in let ghcjs865 = self.callPackage ../compiler/ghcjs/ghcjs.nix {
                ghcjsSrcJson = ../compiler/ghcjs/ghcjs-src.json;
                ghcjsVersion =  "8.6.0.1";
                ghc = buildGHC;
                cabal-install = self.buildPackages.haskell-nix.cabal-install;
                # The alex from the bootstrap packages is apparently broken, and will fail with something like:
                # > alex: /nix/store/f7b78rg9pmqgvxvsqfzh1przp7pxii5a-alex-3.2.4-exe-alex/share/x86_64-osx-ghc-8.4.4/alex-3.2.4-1pf5faR9dBuJ8mryql0DoA-alex/AlexTemplate-ghc-nopred: openFile: does not exist (No such file or directory)
                # inherit (self.buildPackages.haskell-nix.bootstrap.packages) alex happy;
            }; in let targetPrefix = "js-unknown-ghcjs-"; in self.runCommand "${targetPrefix}ghc-8.6.5" {
                passthru = {
                    inherit targetPrefix;
                    version = "8.6.5";
                    isHaskellNixCompiler = true;
                    inherit (ghcjs865) configured-src;
                    inherit buildGHC;
                    extraConfigureFlags = [
                        "--ghcjs"
                        "--with-ghcjs=${targetPrefix}ghc" "--with-ghcjs-pkg=${targetPrefix}ghc-pkg"
                        # setting gcc is stupid. non-emscripten ghcjs has no cc.
                        # however cabal insists on compiling the c sources. m(
                        "--with-gcc=${self.buildPackages.stdenv.cc}/bin/cc"
                    ];
                };
                # note: we'll use the buildGHCs `hsc2hs`, ghcjss wrapper just horribly breaks in this nix setup.
            } (''
                mkdir -p $out/bin
                cd $out/bin
                ln -s ${ghcjs865}/bin/ghcjs ${targetPrefix}ghc
                ln -s ${ghcjs865}/bin/ghcjs-pkg ${targetPrefix}ghc-pkg
                ln -s ${buildGHC}/bin/hsc2hs ${targetPrefix}hsc2hs
                cd ..
                mkdir lib
                cd lib
                cp -R ${ghcjs865}/lib/ghcjs-8.6.5 ${targetPrefix}ghc-8.6.5
            '' + installDeps targetPrefix);
        })));

    ghc = self.haskell-nix.compiler.ghc865;
    inherit (self.buildPackages.haskell-nix.bootstrap.packages) cabal-install alex happy;

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
    bootstrap = with self.haskell-nix; let ghc = self.buildPackages.haskell-nix.bootstrap.compiler.ghc844; in {
        # XXX: import ../. will throw away all other overlays, config values, ...
        #      this is not ideal!
        # get binary compilers for bootstrapping.  We'll put the eventual proper
        # compilers into the same place where nix expects them.
        # We mark these compilers as boot compilers to make sure they are only used
        # where a boot compiler is expected.
        compiler = builtins.mapAttrs (_: v:
            v.overrideAttrs (drv: {
              postInstall = (drv.postInstall or "") + installDeps "";
            }) // {
              isHaskellNixBootCompiler = true;
            }
          )
          (import ../compiler/old-ghc-nix { pkgs = self; });

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
                # Only a boot compiler is suitable here
                ghc = ghc // { isHaskellNixCompiler = ghc.isHaskellNixBootCompiler; };
                inherit (bootstrap.packages) hpack;
            };

            # now that we have nix-tools and hpack, we can just
            # use `hackage-package` to build any package from
            # hackage with haskell.nix.  For alex and happy we
            # need to use the boot strap compiler as we need them
            # to build ghcs from source.
            alex-project = hackage-project {
                # Only a boot compiler is suitable here
                ghc = ghc // { isHaskellNixCompiler = ghc.isHaskellNixBootCompiler; };
                inherit (bootstrap.packages) cabal-install nix-tools hpack;
                name = "alex"; version = "3.2.4";
                index-state = "2019-10-20T00:00:00Z";
                plan-sha256 = "1adn8s46msqm2rl6yf01z2r81maa2001qh441j491gpmc3ki36n0";
                materialized = ../materialized/alex;
            };
            alex = bootstrap.packages.alex-project.hsPkgs.alex.components.exes.alex;
            happy-project = hackage-project {
                # Only a boot compiler is suitable here
                ghc = ghc // { isHaskellNixCompiler = ghc.isHaskellNixBootCompiler; };
                inherit (bootstrap.packages) cabal-install nix-tools hpack;
                name = "happy"; version = "1.19.11";
                index-state = "2019-10-20T00:00:00Z";
                plan-sha256 = "0swpwhlym4p3209qv90mfgq6zsaw99ipznm4pvd32mxzwq9s5q8i";
                materialized = ../materialized/happy;
            };
            happy = bootstrap.packages.happy-project.hsPkgs.happy.components.exes.happy;
            hscolour-project = hackage-project {
                # Only a boot compiler is suitable here
                ghc = ghc // { isHaskellNixCompiler = ghc.isHaskellNixBootCompiler; };
                inherit (bootstrap.packages) cabal-install nix-tools hpack;
                name = "hscolour"; version = "1.24.4";
                index-state = "2019-10-20T00:00:00Z";
                plan-sha256 = "0cnkczsh1xy7cc60q3blwa51qrjhf5mc89s34y9ab3x702a26b75";
                materialized = ../materialized/hscolour;
            };
            hscolour = bootstrap.packages.hscolour-project.hsPkgs.hscolour.components.exes.HsColour;
        };
    };
  };
}
