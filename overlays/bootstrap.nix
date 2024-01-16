final: prev:
let
    # For each architecture, what GHC version we should use for bootstrapping.
    buildBootstrapper =
        if final.stdenv.buildPlatform.isAarch64 && final.stdenv.buildPlatform.isDarwin
        then {
            compilerNixName = "ghc8107";
        }
        else {
            compilerNixName = "ghc884";
        };
    latestVer = {
      "8.4" = "8.4.4";
      "8.6" = "8.6.5";
      "8.8" = "8.8.4";
      "8.10" = "8.10.7";
      "9.0" = "9.0.2";
      "9.2" = "9.2.8";
      "9.4" = "9.4.8";
      "9.6" = "9.6.4";
      "9.8" = "9.8.1";
    };
    gitInputs = {
      ghc98X = "9.8.1";
      ghc99 = "9.9";
    };
    versionToNixName = v: "ghc${builtins.replaceStrings ["."] [""] v}";
    compilerNameMap =
      builtins.mapAttrs (source-name: v:
        versionToNixName "${v}.${builtins.substring 0 8 final.haskell-nix.sources.${source-name}.lastModifiedDate}")
          gitInputs //
      builtins.listToAttrs (map (v:
        { name = versionToNixName v; value = versionToNixName latestVer.${v}; })
          (builtins.attrNames latestVer));
    traceWarnOld = v: x:
      let
        bootstrapGhc = final.buildPackages.haskell-nix.bootstrap.compiler."${buildBootstrapper.compilerNixName}";
      in
      if builtins.compareVersions x.src-spec.version bootstrapGhc.version < 0 then
          throw "Desired GHC (${x.src-spec.version}) is older than the bootstrap GHC (${bootstrapGhc.version}) for this platform (${final.stdenv.targetPlatform.config})."
      # There is no binary for aarch64-linux ghc 8.8.4 so don't warn about 8.8.3 not being the latest version
      else if x.src-spec.version == "8.8.3" && (final.stdenv.targetPlatform.isAarch64 || final.stdenv.buildPlatform.isAarch64)
        then x
      else if builtins.compareVersions x.src-spec.version latestVer.${v} < 0
        then builtins.trace
          "WARNING: ${x.src-spec.version} is out of date, consider using ${latestVer.${v}}." x
      else x;
    errorOldGhcjs = v: up: throw "ghcjs ${v} is no longer supported by haskell.nix. Consider using ${latestVer.${up}}";
in {
  haskell-nix = prev.haskell-nix // {
    # This can be used to map a compiler-nix-name from a shorter form.
    # For instance it will map:
    #   "ghc810" -> "ghc8107"
    #   "ghc99" -> "ghc9920230909" (uses last modified date of the git repo)
    inherit compilerNameMap;
    resolve-compiler-name = name: final.haskell-nix.compilerNameMap.${name} or name;
    # Use this to disable the existing haskell infra structure for testing purposes
    compiler =
        let bootPkgs = {
                ghc = final.buildPackages.buildPackages.haskell-nix.bootstrap.compiler."${buildBootstrapper.compilerNixName}";
                alex = final.haskell-nix.bootstrap.packages.alex-unchecked;
                happy = final.haskell-nix.bootstrap.packages.happy-unchecked;
                hscolour = final.haskell-nix.bootstrap.packages.hscolour-unchecked;
            };
            bootPkgsGhc94 = bootPkgs // {
                alex = final.buildPackages.haskell-nix.tool "ghc902" "alex" {
                  compilerSelection = p: p.haskell.compiler;
                  version = "3.2.7.1";
                  index-state = final.haskell-nix.internalHackageIndexState;
                  materialized = ../materialized/alex-3.2.7.1;
                };
                happy = final.buildPackages.haskell-nix.tool "ghc902" "happy" {
                  compilerSelection = p: p.haskell.compiler;
                  version = "1.20.0";
                  index-state = final.haskell-nix.internalHackageIndexState;
                  materialized = ../materialized/happy-1.20.0;
                };
            };
            sphinx = with final.buildPackages; python3Packages.sphinx_1_7_9 or sphinx_5;

            D5123-patch = final.fetchpatch rec { # https://phabricator.haskell.org/D5123
                url = "http://tarballs.nixos.org/sha256/${sha256}";
                name = "D5123.diff";
                sha256 = "0nhqwdamf2y4gbwqxcgjxs0kqx23w9gv5kj0zv6450dq19rji82n";
            };
            haddock-900-patch = final.fetchpatch { # https://github.com/haskell/haddock/issues/900
                url = "https://patch-diff.githubusercontent.com/raw/haskell/haddock/pull/983.diff";
                name = "loadpluginsinmodules.diff";
                sha256 = "0bvvv0zsfq2581zsir97zfkggc1kkircbbajc2fz3b169ycpbha1";
                extraPrefix = "utils/haddock/";
                stripLen = 1;
            };
            ghc-patches = version: let
                # Returns true iff this derivation's version is strictly older than ver.
                versionLessThan = ver: builtins.compareVersions ver version == 1;
                # Returns true iff this derivation's version is greater than or equal to ver.
                versionAtLeast = ver: !versionLessThan ver;
                fromUntil = start: end: final.lib.optional (versionAtLeast start && versionLessThan end);
                until = end: final.lib.optional (versionLessThan end);
                always = final.lib.optional true;
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
                ++ fromUntil "8.6"   "8.10"  ./patches/ghc/mistuke-ghc-err_clean_up_error_handler-8ab1a89af89848f1713e6849f189de66c0ed7898.diff # this is part of Phyx- revamped io-manager.
                ++ until             "8.10"  ./patches/ghc/ghc-add-keepCAFs-to-rts.patch                         # https://gitlab.haskell.org/ghc/ghc/merge_requests/950  -- open
                ++ until             "8.10"  ./patches/ghc/lowercase-8.6.patch                                   # https://gitlab.haskell.org/ghc/ghc/merge_requests/949  -- merged; ghc-8.8.1
                ++ always                    ./patches/ghc/dll-loader-8.4.2.patch                                # https://gitlab.haskell.org/ghc/ghc/merge_requests/949  -- open
                ++ until             "9.2"   ./patches/ghc/ghc-8.4.3-Cabal2201-no-hackage-tests.patch            # ?
                ++ until             "8.10"  ./patches/ghc/MR948--32bit-cross-th.patch                           # https://gitlab.haskell.org/ghc/ghc/merge_requests/948  -- open
                ++ fromUntil "8.8"   "9.2"   ./patches/ghc/cabal-host.patch                                      # https://github.com/haskell/cabal/issues/5887
                ++ fromUntil "9.2"   "9.3"   ./patches/ghc/ghc-9.2-cabal-host.patch                              # https://github.com/haskell/cabal/issues/5887
                ++ fromUntil "8.6.4" "8.8"   ./patches/ghc/ghc-8.6.4-prim-no-arm-atomics.patch
                ++ fromUntil "8.6.4" "8.8"   ./patches/ghc/global-offset-table.patch
                ++ fromUntil "8.6.4" "8.8"   ./patches/ghc/global-offset-table-2.patch
                ++ until             "9.0"   ./patches/ghc/respect-ar-path.patch
                ++ until             "8.10"  ./patches/ghc/MR2537-use-one-shot-kqueue-on-macos.patch
                ++ final.lib.optional (version == "8.6.3") ./patches/ghc/T16057--ghci-doa-on-windows.patch
                ++ final.lib.optional (version == "8.6.3") ./patches/ghc/ghc-8.6.3-reinstallable-lib-ghc.patch
                ++ final.lib.optional (version == "8.6.4") ./patches/ghc/ghc-8.6.4-reinstallable-lib-ghc.patch
                ++ final.lib.optional (version == "8.6.5") ./patches/ghc/ghc-8.6.5-reinstallable-lib-ghc.patch
                ++ fromUntil "8.6.5" "8.9"   ./patches/ghc/ghc-8.6.5-atomic-arm-arch.patch
                ++ final.lib.optional (version == "8.6.5") ./patches/ghc/MR3214-writable-rel-ro-data.patch
                ++ final.lib.optional (version == "8.8.1") ./patches/ghc/ghc-8.8.1-reinstallable-lib-ghc.patch
                ++ fromUntil "8.8.2" "8.9"                ./patches/ghc/ghc-8.8.2-reinstallable-lib-ghc.patch
                ++ final.lib.optional (version == "8.6.4") ./patches/ghc/ghc-8.6.4-better-plusSimplCountErrors.patch
                ++ final.lib.optional (versionAtLeast "8.6.4" && versionLessThan "9.0" && final.stdenv.isDarwin) ./patches/ghc/ghc-macOS-loadArchive-fix.patch
                ++ final.lib.optional (versionAtLeast "9.0.0" && versionLessThan "9.2" && final.stdenv.isDarwin) ./patches/ghc/ghc-9.0-macOS-loadArchive-fix.patch
                ++ final.lib.optional (versionAtLeast "9.2.0" && versionLessThan "9.3" && final.stdenv.isDarwin) ./patches/ghc/ghc-9.2-macOS-loadArchive-fix.patch
                ++ final.lib.optional (versionAtLeast "8.4.4" && versionLessThan "8.10" && final.stdenv.isDarwin) ./patches/ghc/ghc-darwin-gcc-version-fix.patch
                ++ final.lib.optional (versionAtLeast "8.10.1" && versionLessThan "9.0.2" && final.stdenv.isDarwin) ./patches/ghc/ghc-8.10-darwin-gcc-version-fix.patch
                # backport of https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3227
                # the first one is a prerequisite.
                # both are trimmed to only include the make build system part and not the
                # hadrian one.
                ++ fromUntil "8.8"  "8.10.2"   ./patches/ghc/bec76733b818b0489ffea0834ab6b1560207577c.patch
                ++ fromUntil "8.8"  "8.8.4"    ./patches/ghc/67738db10010fd28a8e997b5c8f83ea591b88a0e.patch
                ++ fromUntil "8.10" "8.10.2"   ./patches/ghc/67738db10010fd28a8e997b5c8f83ea591b88a0e.patch
                ++ final.lib.optional (versionAtLeast "8.6.4" && versionLessThan "8.8") ./patches/ghc/ghc-no-system-linker.patch

                ++ fromUntil "8.10.2" "8.10.3" ./patches/ghc/MR3714-backported-to-8.10.2.patch

                # See https://github.com/input-output-hk/haskell.nix/issues/1027
                ++ final.lib.optional (versionAtLeast "8.10.3" && versionLessThan "9.2" && final.stdenv.targetPlatform.isAarch64) ./patches/ghc/ghc-8.10-3434.patch
                ++ final.lib.optional (versionAtLeast "9.2.1"  && versionLessThan "9.3" && final.stdenv.targetPlatform.isAarch64) ./patches/ghc/ghc-9.2-3434.patch

                ++ fromUntil "8.10.1" "9.4"    ./patches/ghc/ghc-acrt-iob-func.patch
                ++ fromUntil "8.10.1" "9.2"    ./patches/ghc/ghc-mprotect-nonzero-len.patch
                ++ fromUntil "9.2"    "9.2.8"  ./patches/ghc/ghc-9.2-fix-m32_allocator_init-10453.patch # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10453
                ++ fromUntil "9.2"    "9.2.8"  ./patches/ghc/ghc-9.2-fix-m32_allocator_init-10453-2.patch
                ++ fromUntil "9.2"    "9.4"    ./patches/ghc/ghc-9.2-bignum-expose-backendName.patch # https://gitlab.haskell.org/ghc/ghc/-/commit/bc498fdfa482dfe796e3a12ac8f9d71913930740

                ++ fromUntil "8.10.1" "8.10.3" ./patches/ghc/ghc-8.10-ubxt.patch
                ++ fromUntil "8.10.3" "8.10.5" ./patches/ghc/ghc-8.10.3-ubxt.patch
                ++ fromUntil "8.10.5" "9.0"    ./patches/ghc/ghc-8.10.5-ubxt.patch
                ++ final.lib.optionals (!final.stdenv.targetPlatform.isDarwin)
                  (  fromUntil "8.6.4"  "9.2"    ./patches/ghc/Cabal-3886.patch
                  ++ fromUntil "9.2"    "9.3"    ./patches/ghc/ghc-9.2-Cabal-3886.patch)

                ++ fromUntil "8.10.3" "8.10.5" ./patches/ghc/ghc-8.10.3-rts-make-markLiveObject-thread-safe.patch
                ++ final.lib.optionals final.stdenv.targetPlatform.isWindows
                  (fromUntil "8.10.4" "9.3"    ./patches/ghc/ghc-8.10-z-drive-fix.patch)
                ++ fromUntil "8.6.5"  "9.4"    ./patches/ghc/ghc-8.10-windows-add-dependent-file.patch
                ++ fromUntil "8.10.1" "9.0"    ./patches/ghc/Cabal-unbreak-GHCJS.patch
                ++ until              "8.10.5" ./patches/ghc/AC_PROG_CC_99.patch
                ++ fromUntil "9.0.1"  "9.0.2"  ./patches/ghc/AC_PROG_CC_99.patch
                ++ fromUntil "8.10.5" "8.10.6" ./patches/ghc/ghc-8.10.5-add-rts-exports.patch
                ++ final.lib.optionals final.stdenv.hostPlatform.isDarwin
                  (fromUntil "8.10.5" "8.10.6" ./patches/ghc/ghc-8.10.5-darwin-allocateExec.patch)
                ++ until              "8.10.6" ./patches/ghc/Sphinx_Unicode_Error.patch
                ++ fromUntil "9.0.2"  "9.2.2"  ./patches/ghc/ghc-9.2.1-xattr-fix.patch      # Problem was backported to 9.0.2
                ++ fromUntil "8.10"   "9.3"    ./patches/ghc/MR6654-nonmoving-maxmem.patch  # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6654
                ++ fromUntil "8.10"   "9.0.2"  ./patches/ghc/MR6617-nonmoving-mvar.patch    # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6617
                ++ fromUntil "8.10"   "9.0.2"  ./patches/ghc/MR6595-nonmoving-mutvar.patch  # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6595
                ++ fromUntil "8.10"   "9.2"    ./patches/ghc/ghc-8.10-global-unique-counters-in-rts.patch # backport of https://gitlab.haskell.org/ghc/ghc/-/commit/9a28680d2e23e7b25dd7254a439aea31dfae32d5
                ++ fromUntil "9.2"    "9.3"    ./patches/ghc/ghc-9.2-global-unique-counters-in-rts.patch # backport of https://gitlab.haskell.org/ghc/ghc/-/commit/9a28680d2e23e7b25dd7254a439aea31dfae32d5
                ++ fromUntil "8.10"   "9.1"    ./patches/ghc/issue-18708.patch              # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6554
                ++ fromUntil "9.2.2"  "9.3"    ./patches/ghc/ghc-9.2.2-fix-warnings-building-with-self.patch # https://gitlab.haskell.org/ghc/ghc/-/commit/c41c478eb9003eaa9fc8081a0039652448124f5d
                ++ fromUntil "8.6.5"  "9.5"    ./patches/ghc/ghc-hpc-response-files.patch   # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8194
                ++ fromUntil "9.2"    "9.10"   ./patches/ghc/sanity-check-find-file-name.patch
                ++ fromUntil "8.0"    "9.0"    ./patches/ghc/dont-mark-evacuate_large-as-inline.patch
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4.1"  "9.4.5"  ./patches/ghc/ghc-9.4-hadrian-win-cross.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4.7"  "9.4.9"  ./patches/ghc/ghc-9.8-hadrian-win-cross.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.6.3"  "9.10"   ./patches/ghc/ghc-9.8-hadrian-win-cross.patch)
                # support R_X86_64_PC64 (ELF constant 24) - IMAGE_REL_AMD64_SREL32 (PE constant 14), which seems to appear with 9.6 more frequently, and
                # results in "unhandled PEi386 relocation type 14".
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4.1"  "9.10"   ./patches/ghc/win-reloc-x86_64-pc64.patch)
                # ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4.1"  "9.10"   ./patches/ghc/Win32-depends-on-mingwex.patch)
                # if the host system provides ucrt (e.g. wine with ucrtbase.dll), we may end up linking against symbols from ucrtbase, instead of msvcrt,
                # thus leading to broken code.  E.g. the handles we create and hand to wine will all be busted, because they come from one and are processed
                # by another crt.
                ++ final.lib.optionals (final.stdenv.targetPlatform.libc != "ucrt") (
                    final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "8.10"   "9.8"   ./patches/ghc/win-linker-no-ucrt.patch)
                  # Nixos/nixpkgs is mscvrt for now, thus we must disable ucrt in ghc, otherwise we end up with broken linking.
                  ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4.1"  "9.6"    ./patches/ghc/no-ucrt-9.4.patch)
                  ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.6.1"  "9.6.3"  ./patches/ghc/no-ucrt-9.6.patch)
                  ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.6.3"  "9.8"    ./patches/ghc/no-ucrt-9.6.3.patch)
                  ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.8"    "9.9"    ./patches/ghc/no-ucrt-9.8.patch)
                  ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.9"    "9.9.20231203" ./patches/ghc/no-ucrt-9.9.patch)
                )
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4.7"  "9.5"    ./patches/ghc/revert-289547580b6f2808ee123f106c3118b716486d5b.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.6.3"  "9.10"   ./patches/ghc/revert-289547580b6f2808ee123f106c3118b716486d5b.patch)
                # the following is needed for cardano-prelude as it uses closure_sizeW :-/
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4"    "9.10"   ./patches/ghc/win-add-closure_sizeW-to-rtssyms.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "8.10.7" "8.11"   ./patches/ghc/ghc-8.10-win-add-tzset-to-rtssyms.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.0"    "9.3"    ./patches/ghc/ghc-9.2-win-add-tzset-to-rtssyms.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4"    "9.10"   ./patches/ghc/win-add-tzset-to-rtssyms.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4.1"  "9.4.7"  ./patches/ghc/win-linker-no-null-deref.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4.7"  "9.4.8"  ./patches/ghc/win-linker-no-null-deref-9.6.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.4.1"  "9.6"    ./patches/ghc/ghc-9.4-drop-mingwex-from-base.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.6.1"  "9.6.3"  ./patches/ghc/win-linker-no-null-deref.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.6.3"  "9.6.4" ./patches/ghc/win-linker-no-null-deref-9.6.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.8.1"  "9.9.20231203" ./patches/ghc/win-linker-no-null-deref-9.6.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "8.10.7" "9.0"    ./patches/ghc/ghc-8.10-windres-invocation.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isWindows) (fromUntil "9.0"    "9.4"    ./patches/ghc/ghc-9.0-windres-invocation.patch)
                ++ fromUntil "9.4.5"  "9.4.9"  ./patches/ghc/ghc-9.4.5-include-order-fix.patch
                ++ fromUntil "9.6.2"  "9.8"    ./patches/ghc/ghc-9.4.5-include-order-fix.patch
                ++ fromUntil "9.6.1"  "9.9.20231203" ./patches/ghc/MR10116.patch
                ++ final.lib.optionals (final.stdenv.buildPlatform == final.stdenv.targetPlatform) (fromUntil "9.4.1" "9.6" ./patches/ghc/hadrian-build-deriveConstants-genprimopcode-ghc94.patch)
                ++ final.lib.optionals (final.stdenv.buildPlatform == final.stdenv.targetPlatform) (fromUntil "9.6.1" "9.10" ./patches/ghc/hadrian-build-deriveConstants-genprimopcode.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isGhcjs) (fromUntil "9.6.1"  "9.6.3"  ./patches/ghc/ghc-9.6-Merge-libiserv-with-ghci.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isGhcjs) (fromUntil "9.6.3"  "9.8"    ./patches/ghc/ghc-9.6.3-Merge-libiserv-with-ghci.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isGhcjs) (fromUntil "9.6.1"  "9.8"    ./patches/ghc/ghc-9.6-Assorted-changes-to-avoid-head-tail.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isGhcjs) (fromUntil "9.6.1"  "9.6.3"  ./patches/ghc/ghc-9.6-JS-implement-TH-support.patch)
                ++ final.lib.optionals (final.stdenv.targetPlatform.isGhcjs) (fromUntil "9.6.3"  "9.8"    ./patches/ghc/ghc-9.6.3-JS-implement-TH-support.patch)
                ++ fromUntil "9.8.1"  "9.8.2"  ./patches/ghc/ghc-9.8-cabal-c-soures-fix.patch
                ++ fromUntil "9.6.3"  "9.9"    ./patches/ghc/ghc-9.6.3-Cabal-9384.patch

                # the following is a partial reversal of https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4391, to address haskell.nix#1227
                ++ final.lib.optional (versionAtLeast "8.10.6" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAarch64) ./patches/ghc/mmap-next.patch
                ++ final.lib.optional (versionAtLeast "8.10.6" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAarch64) ./patches/ghc/m32_alloc.patch
                ++ final.lib.optional (versionAtLeast "8.10.6" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAndroid) ./patches/ghc/rts-android-jemalloc-qemu.patch
                ++ final.lib.optional (versionAtLeast "8.10.6" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAndroid) ./patches/ghc/stack-protector-symbols.patch
                ++ final.lib.optional (versionAtLeast "8.10.6" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAndroid) ./patches/ghc/libraries-prim-os-android.patch
                ++ final.lib.optional (versionAtLeast "8.10.6" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAndroid) ./patches/ghc/ghc-rts-linker-condbr.patch
                ++ final.lib.optional (versionAtLeast "8.10.6" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAndroid) ./patches/ghc/ghc-8.10.7-linker-weak-and-common.patch
                ++ final.lib.optional (versionAtLeast "8.10.6" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAndroid) ./patches/ghc/libc-memory-symbols.patch
                ++ final.lib.optional (versionAtLeast "8.10.6" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAndroid) ./patches/ghc/android-base-needs-iconv.patch
                ++ final.lib.optional (versionAtLeast "8.10"   && versionLessThan "9.4" && final.stdenv.targetPlatform != final.stdenv.hostPlatform) ./patches/ghc/ghc-make-stage-1-lib-ghc.patch
                ++ final.lib.optional (versionAtLeast "8.10"   && versionLessThan "9.0" && final.stdenv.targetPlatform.isAarch64) ./patches/ghc/ghc-8.10-better-symbol-addr-debug.patch
                ++ final.lib.optional (versionAtLeast "8.10"   && versionLessThan "9.0" && final.stdenv.targetPlatform.isAarch64) ./patches/ghc/ghc-8.10-aarch64-handle-none-rela.patch
                ++ final.lib.optional (versionAtLeast "9.0"                             && final.stdenv.targetPlatform.isAarch64) ./patches/ghc/ghc-9.0-better-symbol-addr-debug.patch
                ++ final.lib.optional (versionAtLeast "9.0"                             && final.stdenv.targetPlatform.isAarch64) ./patches/ghc/ghc-9.0-aarch64-handle-none-rela.patch
                ++ final.lib.optional (versionAtLeast "9.6.3"  && versionLessThan "9.6.4" && final.stdenv.targetPlatform.isWindows) ./patches/ghc/ghc-9.6-hadrian-splitsections.patch
                ++ final.lib.optional (versionAtLeast "9.8.1"  && versionLessThan "9.9" && final.stdenv.targetPlatform.isWindows) ./patches/ghc/ghc-9.6-hadrian-splitsections.patch
                # this patch was backported to 9.4.8
                ++ final.lib.optional (versionAtLeast "9.4"    && versionLessThan "9.4.8" && final.stdenv.targetPlatform.isWindows) ./patches/ghc/ghc-9.6-fix-code-symbol-jumps.patch
                ++ final.lib.optional (versionAtLeast "9.6"    && versionLessThan "9.6.4" && final.stdenv.targetPlatform.isWindows) ./patches/ghc/ghc-9.6-fix-code-symbol-jumps.patch
                ++ final.lib.optional (versionAtLeast "9.8"    && versionLessThan "9.9.20231203" && final.stdenv.targetPlatform.isWindows) ./patches/ghc/ghc-9.6-fix-code-symbol-jumps.patch
                # this one is to allow linking extra symbols from iserv.
                # ++ fromUntil "9.6.1" "9.10"                                                                                       ./patches/ghc/iserv-syms.patch
                # Fix the bad fixups: https://gitlab.haskell.org/ghc/ghc/-/commit/2adc050857a9c1b992040fbfd55fbe65b2851b19
                ++ final.lib.optional (versionAtLeast "9.6"    && versionLessThan "9.6.4" && final.stdenv.targetPlatform.isAarch64) ./patches/ghc/2adc050857a9c1b992040fbfd55fbe65b2851b19.patch
                ++ final.lib.optional (versionAtLeast "8.10.7" && versionLessThan "9.0" && final.stdenv.targetPlatform.isAarch64 && final.stdenv.targetPlatform.isMusl && final.stdenv.targetPlatform != final.stdenv.hostPlatform) ./patches/ghc/ghc-8.10-aarch64-musl-gettimeofday.patch
                ;
        in ({
            ghc865 = final.callPackage ../compiler/ghc (traceWarnOld "8.6" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc865; };

                bootPkgs = bootPkgs // {
                  # GHC 8.6.5 and earlier need happy 1.19.11
                  happy = final.haskell-nix.bootstrap.packages.happy-old-unchecked;
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc865
                    else final.buildPackages.buildPackages.haskell.compiler.ghc865;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_6;
                llvmPackages = final.llvmPackages_6;

                src-spec = rec {
                    version = "8.6.5";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0qg3zsmbk4rkwkc3jpas3zs74qaxmw4sp4v1mhsbj0a0dzls2jjd";
                };

                ghc-patches = ghc-patches "8.6.5"
                            ++ [ D5123-patch haddock-900-patch ];
            });
            ghc881 = final.callPackage ../compiler/ghc (traceWarnOld "8.8" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc881; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc881
                    else final.buildPackages.buildPackages.haskell.compiler.ghc884;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_7;
                llvmPackages = final.llvmPackages_7;

                src-spec = rec {
                    version = "8.8.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "06kj4fhvijinjafiy4s873n60qly323rdlz9bmc79nhlp3cq72lh";
                };

                ghc-patches = ghc-patches "8.8.1";
            });
            ghc882 = final.callPackage ../compiler/ghc (traceWarnOld "8.8" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc882; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc882
                    else final.buildPackages.buildPackages.haskell.compiler.ghc884;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_7;
                llvmPackages = final.llvmPackages_7;

                src-spec = rec {
                    version = "8.8.2";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "02qa6wgjpxgakg7hv4zfdlrx9k7zxa5i02wnr6y9fsv8j16sbkh1";
                };

                ghc-patches = ghc-patches "8.8.2";
            });
            ghc883 = final.callPackage ../compiler/ghc (traceWarnOld "8.8" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc883; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc883
                    else final.buildPackages.buildPackages.haskell.compiler.ghc884;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_7;
                llvmPackages = final.llvmPackages_7;

                src-spec = rec {
                    version = "8.8.3";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "128g932i3wix6ic03v04nh5755vyjiidzri9iybwad72yfmc1p70";
                };

                ghc-patches = ghc-patches "8.8.3";
            });
            ghc884 = final.callPackage ../compiler/ghc (traceWarnOld "8.8" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc884; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc884
                    else final.buildPackages.buildPackages.haskell.compiler.ghc884;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_7;
                llvmPackages = final.llvmPackages_7;

                src-spec = rec {
                    version = "8.8.4";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0bgwbxxvdn56l91bp9p5d083gzcfdi6z8l8b17qzjpr3n8w5wl7h";
                };

                ghc-patches = ghc-patches "8.8.4";
            });
            ghc8101 = final.callPackage ../compiler/ghc (traceWarnOld "8.10" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc8101; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc8101
                    else final.buildPackages.buildPackages.haskell.compiler.ghc8107;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_9;
                llvmPackages = final.llvmPackages_9;

                src-spec = rec {
                    version = "8.10.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "1xgdl6ig5jzli3bg054vfryfkg0y6wggf68g66c32sr67bw0ffsf";
                };

                ghc-patches = ghc-patches "8.10.1";
            });
            ghc8102 = final.callPackage ../compiler/ghc (traceWarnOld "8.10" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc8102; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc8102
                    else final.buildPackages.buildPackages.haskell.compiler.ghc8107;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_9;
                llvmPackages = final.llvmPackages_9;

                src-spec = rec {
                    version = "8.10.2";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "02w8n085bw38vyp694j0lfk5wcnwkdaj7hhp0saj71x74533lmww";
                };

                ghc-patches = ghc-patches "8.10.2";
            });
            ghc8103 = final.callPackage ../compiler/ghc (traceWarnOld "8.10" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc8103; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc8103
                    else final.buildPackages.buildPackages.haskell.compiler.ghc8107;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_9;
                llvmPackages = final.llvmPackages_9;

                src-spec = rec {
                    version = "8.10.3";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0cdrdvs5qnqr93cr9zvrlfjv2xr671kjjghnsw4afa4hahcq7p6c";
                };

                ghc-patches = ghc-patches "8.10.3";
            });
            ghc8104 = final.callPackage ../compiler/ghc (traceWarnOld "8.10" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc8104; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc8104
                    else final.buildPackages.buildPackages.haskell.compiler.ghc8107;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_9;
                llvmPackages = final.llvmPackages_9;

                src-spec = rec {
                    version = "8.10.4";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "03li4k10hxgyxcdyyz2092wx09spr1599hi0sxbh4m889qdqgbsj";
                };

                ghc-patches = ghc-patches "8.10.4";
            });
            ghc8105 = final.callPackage ../compiler/ghc (traceWarnOld "8.10" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc8105; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc8105
                    else final.buildPackages.buildPackages.haskell.compiler.ghc8107;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_9;
                llvmPackages = final.llvmPackages_9;

                src-spec = rec {
                    version = "8.10.5";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "0vq7wch0wfvy2b5dbi308lq5225vf691n95m19c9igagdvql22gi";
                };

                ghc-patches = ghc-patches "8.10.5";
            });
            ghc8106 = final.callPackage ../compiler/ghc (traceWarnOld "8.10" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc8106; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc8106
                    else final.buildPackages.buildPackages.haskell.compiler.ghc8107;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_9;
                llvmPackages = final.llvmPackages_9;

                src-spec = rec {
                    version = "8.10.6";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "1s8cc50whb0qsgnmq6p40ir5yzp3nm3x0as9q518nh1klmrbmbs3";
                };

                ghc-patches = ghc-patches "8.10.6";
            });
            ghc8107 = final.callPackage ../compiler/ghc (traceWarnOld "8.10" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc8107; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107
                    else final.buildPackages.buildPackages.haskell.compiler.ghc8107;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "8.10.7";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "179ws2q0dinl1a39wm9j37xzwm84zfz3c5543vz8v479khigdvp3";
                };

                ghc-patches = ghc-patches "8.10.7";
            });
            ghc901 = final.callPackage ../compiler/ghc (traceWarnOld "9.0" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc901; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc901
                    else final.buildPackages.buildPackages.haskell.compiler.ghc902;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_9;
                llvmPackages = final.llvmPackages_9;

                src-spec = rec {
                    version = "9.0.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "1y9mi9bq76z04hmggavrn8jwi1gx92bm3zhx6z69ypq6wha068x5";
                };

                ghc-patches = ghc-patches "9.0.1";
            });
            ghc902 = final.callPackage ../compiler/ghc (traceWarnOld "9.0" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc902; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc902
                    else final.buildPackages.buildPackages.haskell.compiler.ghc902;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_9;
                llvmPackages = final.llvmPackages_9;

                src-spec = rec {
                    version = "9.0.2";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "15wii8can2r3dcl6jjmd50h2jvn7rlmn05zb74d2scj6cfwl43hl";
                };

                ghc-patches = ghc-patches "9.0.2";
            });
            ghc921 = final.callPackage ../compiler/ghc (traceWarnOld "9.2" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc921; };

                bootPkgs = bootPkgs // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.2.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-9EQBL5ehNtmUD3fN/wP9pI+UdeLtD+yWbE01xN9V90Y=";
                };

                ghc-patches = ghc-patches "9.2.1";
            });
            ghc922 = final.callPackage ../compiler/ghc (traceWarnOld "9.2" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc922; };

                bootPkgs = bootPkgs // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.2.2";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-kCRjpMxu5Hmvk1i5+LLuMjewPpNKHqZbbR/PPg10nqY=";
                };

                ghc-patches = ghc-patches "9.2.2";
            });
            ghc923 = final.callPackage ../compiler/ghc (traceWarnOld "9.2" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc923; };

                bootPkgs = bootPkgs // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.2.3";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-UOzcK+8BPlGPmmKhUkXX2w5ECdc3xDsc6nMG/YLhZp4=";
                };

                ghc-patches = ghc-patches "9.2.3";
            });
            ghc924 = final.callPackage ../compiler/ghc (traceWarnOld "9.2" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc924; };

                bootPkgs = bootPkgs // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.2.4";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-FSE4iAZKDsTncj0HXzG4emeM4IUXc9WLRO96o96ZZFg=";
                };

                ghc-patches = ghc-patches "9.2.4";
            });
            ghc925 = final.callPackage ../compiler/ghc (traceWarnOld "9.2" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc925; };

                bootPkgs = bootPkgs // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.2.5";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-BgZ5fRs44tiO4iQ/OOxrmhqpPptXjpXw3pqcCkFEAhw=";
                };

                ghc-patches = ghc-patches "9.2.5";
            });
            ghc926 = final.callPackage ../compiler/ghc (traceWarnOld "9.2" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc926; };

                bootPkgs = bootPkgs // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.2.6";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-elTPA5itSItO0hnhXR0eZMC2h2xDoFZFUN0R8FQNcwU=";
                };

                ghc-patches = ghc-patches "9.2.6";
            });
            ghc927 = final.callPackage ../compiler/ghc (traceWarnOld "9.2" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc927; };

                bootPkgs = bootPkgs // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.2.7";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-olNWehe3NKTA3Q/6KW0zwqW1pUp335iIBqKh4cp+iLg=";
                };

                ghc-patches = ghc-patches "9.2.7";
            });
            ghc928 = final.callPackage ../compiler/ghc (traceWarnOld "9.2" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc928; };

                bootPkgs = bootPkgs // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.2.8";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-XxPReGv0/RL0tF+qN6vttbs/NtXlj32lMH6L/oilZ6E=";
                };

                ghc-patches = ghc-patches "9.2.8";
            });
            ghc941 = final.callPackage ../compiler/ghc (traceWarnOld "9.4" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc941; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc941
                    else final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.4.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-y/7UZAvfAl4zulVDPa+M32mPTgSZrnqADd5EqC5zluM=";
                };

                ghc-patches = ghc-patches "9.4.1";
            });
            ghc942 = final.callPackage ../compiler/ghc (traceWarnOld "9.4" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc942; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc942
                    else final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.4.2";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-cifvO14VoNcLjxpDrsMoZ+KpsthXzA7VVq7tFy1Ns6U";
                };

                ghc-patches = ghc-patches "9.4.2";
            });
            ghc943 = final.callPackage ../compiler/ghc (traceWarnOld "9.4" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc943; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc943
                    else final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.4.3";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-6vY5SVNu3lDuORefIpnVCU65FS2HzG+yF1AGvJjokFo=";
                };

                ghc-patches = ghc-patches "9.4.3";
            });
            ghc944 = final.callPackage ../compiler/ghc (traceWarnOld "9.4" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc944; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc944
                    else final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.4.4";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-6M7yWm3tFTHNp6kEiNDPtteAZX0WY22qWUML4DDNZ+I=";
                };

                ghc-patches = ghc-patches "9.4.4";
            });
            ghc945 = final.callPackage ../compiler/ghc (traceWarnOld "9.4" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc945; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc945
                    else final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.4.5";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-YlbPnK9tbce2Edz7skffLVKOhao50ippjocOWlkOhgE=";
                };

                ghc-patches = ghc-patches "9.4.5";
            });
            ghc947 = final.callPackage ../compiler/ghc (traceWarnOld "9.4" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc947; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc947
                    else final.buildPackages.buildPackages.haskell.compiler.ghc947
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.4.7";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-BndaUrTROsCe3G2rwpn9EeWdiIa7yuRQrzZ7ruJoTI8=";
                };

                ghc-patches = ghc-patches "9.4.7";
            });
            ghc948 = final.callPackage ../compiler/ghc (traceWarnOld "9.4" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc948; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc948
                    else final.buildPackages.buildPackages.haskell.compiler.ghc948
                          or final.buildPackages.buildPackages.haskell.compiler.ghc947
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.4.8";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-C/QH62f+PjwksPTI3qjLY+B/Y8oPds8gWFZRQ1B6uF4=";
                };

                ghc-patches = ghc-patches "9.4.8";
            });
            ghc96020230302 = final.callPackage ../compiler/ghc (traceWarnOld "9.6" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc96020230302; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc96020230302
                    else final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.6.0.20230302";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-Vlj/E1eoL/7PUsYCsareTGPRGEvLzYtjPcxsYaSmNvM=";
                };

                ghc-patches = ghc-patches "9.6.1";
            });
            ghc961 = final.callPackage ../compiler/ghc (traceWarnOld "9.6" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc961; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc961
                    else final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.6.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-/lrJCcuLsIfiNd6X+mOv9HqK5lDvqjeiFA9HgOIfNMs=";
                };

                ghc-patches = ghc-patches "9.6.1";
            });
            ghc962 = final.callPackage ../compiler/ghc (traceWarnOld "9.6" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc962; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc962
                    else final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.6.2";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-G1EMX4dTw7okhRcCxsnafYHcXkf+Pst685x8JhOr8XA=";
                };

                ghc-patches = ghc-patches "9.6.2";
            });
            ghc963 = final.callPackage ../compiler/ghc (traceWarnOld "9.6" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc963; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc963
                    else final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.6.3";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-383me0qlUKC4oam7gQWDXcmZ+tY5fM4z1y/VXSHrd/U=";
                };

                ghc-patches = ghc-patches "9.6.3";
            });
            ghc964 = final.callPackage ../compiler/ghc (traceWarnOld "9.6" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc964; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc964
                    else final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.6.4";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-EL8luLBxdP3ZhotcDFbBfA7x7ctiR7S4ZL6TNlG/1MA=";
                };

                ghc-patches = ghc-patches "9.6.4";
            });
            ghc981 = final.callPackage ../compiler/ghc (traceWarnOld "9.8" {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc981; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc964
                    else final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec = rec {
                    version = "9.8.1";
                    url = "https://downloads.haskell.org/~ghc/${version}/ghc-${version}-src.tar.xz";
                    sha256 = "sha256-svjta39zN5epJDb0/24IilIJExScmpvpBGW0CtHyB1E=";
                };

                ghc-patches = ghc-patches "9.8.1";
            });
        } // (__listToAttrs (final.lib.mapAttrsToList (source-name: ver:
          let
            src = final.haskell-nix.sources.${source-name};
            version-date = __substring 0 8 src.lastModifiedDate;
            version = "${ver}.${version-date}";
            compiler-nix-name = versionToNixName version;
          in {
            name = compiler-nix-name;
            value = final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.${compiler-nix-name}; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc963
                    else final.buildPackages.buildPackages.haskell.compiler.ghc981
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = src;
                src-spec.version = version;
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches version;
                ghc-version-date = version-date;
                ghc-commit-id = src.rev;
            };
          }) gitInputs))
        // final.lib.optionalAttrs (final.stdenv.targetPlatform.isGhcjs or false) (
         if final.stdenv.hostPlatform.isGhcjs
           then throw "An attempt was made to build ghcjs with ghcjs (perhaps use `buildPackages` when refering to ghc)"
           else
                # This will inject `exactDeps` and `envDeps`  into the ghcjs
                # compiler defined below.  This is crucial to build packages
                # with the current use of env and exact Deps.
                (builtins.mapAttrs
                    (_: v: v // {
                        useLLVM = false;
                    })
          ({
            # Better error messages when an unsupported version of ghcjs is used
            ghc844 = errorOldGhcjs "8.4.4" "8.6";
            ghc861 = errorOldGhcjs "8.6.1" "8.6";
            ghc862 = errorOldGhcjs "8.6.2" "8.6";
            ghc863 = errorOldGhcjs "8.6.3" "8.6";
            ghc864 = errorOldGhcjs "8.6.4" "8.6";
            ghc881 = errorOldGhcjs "8.8.1" "8.8";
            ghc882 = errorOldGhcjs "8.8.2" "8.8";
            ghc883 = errorOldGhcjs "8.8.3" "8.8";
            ghc8101 = errorOldGhcjs "8.10.1" "8.10";
            ghc8102 = errorOldGhcjs "8.10.2" "8.10";
            ghc8103 = errorOldGhcjs "8.10.3" "8.10";
            ghc8104 = errorOldGhcjs "8.10.4" "8.10";
            ghc810420210212 = throw "ghcjs for ghc810420210212 is not supported by haskell.nix";
            ghc901 = throw "ghcjs 9.0.1 is not yet supported by haskell.nix";
            ghc865 = let buildGHC = final.buildPackages.haskell-nix.compiler.ghc865;
                in let booted-ghcjs = final.callPackage ../compiler/ghcjs/ghcjs.nix {
                ghcjsSrcJson = ../compiler/ghcjs/ghcjs-src.json;
                ghcjsVersion =  "8.6.0.0.10";
                ghc = buildGHC;
            }; in let targetPrefix = "js-unknown-ghcjs-"; in final.runCommand "${targetPrefix}ghc-8.6.5" {
                nativeBuildInputs = [ final.xorg.lndir ];
                passthru = {
                    inherit targetPrefix;
                    version = "8.6.5";
                    isHaskellNixCompiler = true;
                    enableShared = false;
                    inherit (booted-ghcjs) configured-src bundled-ghcjs project;
                    inherit booted-ghcjs buildGHC;
                    extraConfigureFlags = [
                        "--ghcjs"
                        "--with-ghcjs=${targetPrefix}ghc" "--with-ghcjs-pkg=${targetPrefix}ghc-pkg"
                        "--with-gcc=${final.buildPackages.emscripten}/bin/emcc"
                    ];
                };
                # note: we'll use the buildGHCs `hsc2hs`, ghcjss wrapper just horribly breaks in this nix setup.
            } (''
                mkdir -p $out/bin
                cd $out/bin
                ln -s ${booted-ghcjs}/bin/ghcjs ${targetPrefix}ghc
                ln -s ${booted-ghcjs}/bin/ghcjs-pkg ${targetPrefix}ghc-pkg
                ln -s ${buildGHC}/bin/hsc2hs ${targetPrefix}hsc2hs
                cd ..
                mkdir -p lib/${targetPrefix}ghc-8.6.5
                cd lib
                lndir ${booted-ghcjs}/lib ${targetPrefix}ghc-8.6.5
            '');
            ghc884 = let buildGHC = final.buildPackages.haskell-nix.compiler.ghc884;
                in let booted-ghcjs = final.callPackage ../compiler/ghcjs/ghcjs.nix {
                ghcjsSrcJson = ../compiler/ghcjs/ghcjs88-src.json;
                ghcjsVersion =  "8.8.0.0.1";
                ghc = buildGHC;
                ghcVersion = "8.8.4";
                compiler-nix-name = "ghc884";
            }; in let targetPrefix = "js-unknown-ghcjs-"; in final.runCommand "${targetPrefix}ghc-8.8.4" {
                nativeBuildInputs = [ final.xorg.lndir ];
                passthru = {
                    inherit targetPrefix;
                    version = "8.8.4";
                    isHaskellNixCompiler = true;
                    enableShared = false;
                    inherit (booted-ghcjs) configured-src bundled-ghcjs project;
                    inherit booted-ghcjs buildGHC;
                    extraConfigureFlags = [
                        "--ghcjs"
                        "--with-ghcjs=${targetPrefix}ghc" "--with-ghcjs-pkg=${targetPrefix}ghc-pkg"
                        "--with-gcc=${final.buildPackages.emscripten}/bin/emcc"
                    ];
                };
                # note: we'll use the buildGHCs `hsc2hs`, ghcjss wrapper just horribly breaks in this nix setup.
            } (''
                mkdir -p $out/bin
                cd $out/bin
                ln -s ${booted-ghcjs}/bin/ghcjs ${targetPrefix}ghc
                ln -s ${booted-ghcjs}/bin/ghcjs-pkg ${targetPrefix}ghc-pkg
                ln -s ${buildGHC}/bin/hsc2hs ${targetPrefix}hsc2hs
                cd ..
                mkdir -p lib/${targetPrefix}ghc-8.8.4
                cd lib
                lndir ${booted-ghcjs}/lib ${targetPrefix}ghc-8.8.4
            '');
            ghc8105 = let buildGHC = final.buildPackages.haskell-nix.compiler.ghc8105;
                in let booted-ghcjs = final.callPackage ../compiler/ghcjs/ghcjs.nix {
                ghcjsSrcJson = ../compiler/ghcjs/ghcjs810-src.json;
                ghcjsVersion = "8.10.7"; # Must match the version in the ghcjs.cabal file
                ghc = buildGHC;
                ghcVersion = "8.10.5";
                compiler-nix-name = "ghc8105";
            }; in let targetPrefix = "js-unknown-ghcjs-"; in final.runCommand "${targetPrefix}ghc-8.10.5" {
                nativeBuildInputs = [ final.xorg.lndir ];
                passthru = {
                    inherit targetPrefix;
                    version = "8.10.5";
                    isHaskellNixCompiler = true;
                    enableShared = false;
                    inherit (booted-ghcjs) configured-src bundled-ghcjs project;
                    inherit booted-ghcjs buildGHC;
                    extraConfigureFlags = [
                        "--ghcjs"
                        "--with-ghcjs=${targetPrefix}ghc" "--with-ghcjs-pkg=${targetPrefix}ghc-pkg"
                        "--with-gcc=${final.buildPackages.emscripten}/bin/emcc"
                    ];
                };
                # note: we'll use the buildGHCs `hsc2hs`, ghcjss wrapper just horribly breaks in this nix setup.
            } (''
                mkdir -p $out/bin
                cd $out/bin
                ln -s ${booted-ghcjs}/bin/ghcjs ${targetPrefix}ghc
                ln -s ${booted-ghcjs}/bin/ghcjs-pkg ${targetPrefix}ghc-pkg
                ln -s ${buildGHC}/bin/hsc2hs ${targetPrefix}hsc2hs
                cd ..
                mkdir -p lib/${targetPrefix}ghc-8.10.5
                cd lib
                lndir ${booted-ghcjs}/lib ${targetPrefix}ghc-8.10.5
            '');
            ghc8106 = let buildGHC = final.buildPackages.haskell-nix.compiler.ghc8106;
                in let booted-ghcjs = final.callPackage ../compiler/ghcjs/ghcjs.nix {
                ghcjsSrcJson = ../compiler/ghcjs/ghcjs810-src.json;
                ghcjsVersion =  "8.10.7"; # Must match the version in the ghcjs.cabal file
                ghc = buildGHC;
                ghcVersion = "8.10.6";
                compiler-nix-name = "ghc8106";
            }; in let targetPrefix = "js-unknown-ghcjs-"; in final.runCommand "${targetPrefix}ghc-8.10.6" {
                nativeBuildInputs = [ final.xorg.lndir ];
                passthru = {
                    inherit targetPrefix;
                    version = "8.10.6";
                    isHaskellNixCompiler = true;
                    enableShared = false;
                    inherit (booted-ghcjs) configured-src bundled-ghcjs project;
                    inherit booted-ghcjs buildGHC;
                    extraConfigureFlags = [
                        "--ghcjs"
                        "--with-ghcjs=${targetPrefix}ghc" "--with-ghcjs-pkg=${targetPrefix}ghc-pkg"
                        "--with-gcc=${final.buildPackages.emscripten}/bin/emcc"
                    ];
                };
                # note: we'll use the buildGHCs `hsc2hs`, ghcjss wrapper just horribly breaks in this nix setup.
            } (''
                mkdir -p $out/bin
                cd $out/bin
                ln -s ${booted-ghcjs}/bin/ghcjs ${targetPrefix}ghc
                ln -s ${booted-ghcjs}/bin/ghcjs-pkg ${targetPrefix}ghc-pkg
                ln -s ${buildGHC}/bin/hsc2hs ${targetPrefix}hsc2hs
                cd ..
                mkdir -p lib/${targetPrefix}ghc-8.10.6
                cd lib
                lndir ${booted-ghcjs}/lib ${targetPrefix}ghc-8.10.6
            '');
            ghc8107 = let buildGHC = final.buildPackages.haskell-nix.compiler.ghc8107;
                in let booted-ghcjs = final.callPackage ../compiler/ghcjs/ghcjs.nix {
                ghcjsSrcJson = ../compiler/ghcjs/ghcjs810-src.json;
                ghcjsVersion =  "8.10.7"; # Must match the version in the ghcjs.cabal file
                ghc = buildGHC;
                ghcVersion = "8.10.7";
                compiler-nix-name = "ghc8107";
            }; in let targetPrefix = "js-unknown-ghcjs-"; in final.runCommand "${targetPrefix}ghc-8.10.7" {
                nativeBuildInputs = [ final.xorg.lndir ];
                passthru = {
                    inherit targetPrefix;
                    version = "8.10.7";
                    isHaskellNixCompiler = true;
                    enableShared = false;
                    inherit (booted-ghcjs) configured-src bundled-ghcjs project;
                    inherit booted-ghcjs buildGHC;
                    extraConfigureFlags = [
                        "--ghcjs"
                        "--with-ghcjs=${targetPrefix}ghc" "--with-ghcjs-pkg=${targetPrefix}ghc-pkg"
                        "--with-gcc=${final.buildPackages.emscripten}/bin/emcc"
                    ];
                };
                # note: we'll use the buildGHCs `hsc2hs`, ghcjss wrapper just horribly breaks in this nix setup.
            } (''
                mkdir -p $out/bin
                cd $out/bin
                ln -s ${booted-ghcjs}/bin/ghcjs ${targetPrefix}ghc
                ln -s ${booted-ghcjs}/bin/ghcjs-pkg ${targetPrefix}ghc-pkg
                ln -s ${buildGHC}/bin/hsc2hs ${targetPrefix}hsc2hs
                cd ..
                mkdir -p lib/${targetPrefix}ghc-8.10.7
                cd lib
                lndir ${booted-ghcjs}/lib ${targetPrefix}ghc-8.10.7
            '');
        }))));

    # Both `cabal-install` and `nix-tools` are needed for `cabalProject`
    # to check materialized results.  We need to take care that when
    # it is doing this we do not check the materialization of the
    # tools used or there will be infinite recursion.
    # always has `checkMaterialization = false` to avoid infinite
    # recursion.
    cabal-install-tool = {compiler-nix-name, ...}@args:
      (final.haskell-nix.tool compiler-nix-name "cabal" ({pkgs, ...}: {
        evalPackages = pkgs.buildPackages;
        version = "3.10.1.0";
      } // final.lib.optionalAttrs (__compareVersions final.buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.8.0" >= 0) {
        # It is important not to include this when not needed as it
        # introduces a eval time dependency on the `buildPackages`
        # version of nix-tools (on platforms where we cannot use the
        # static nix-tools).
        cabalProjectLocal = ''
          -- allow newer packages, that are bound to be newer due to
          -- being shipped with a newer compiler.  If you extend this
          -- be very careful to only extend it for absolutely necessary packages
          -- otherwise we risk running into broken build-plans down the line.
          allow-newer: *:base, *:template-haskell, *:bytestring, *:text

          repository head.hackage.ghc.haskell.org
            url: https://ghc.gitlab.haskell.org/head.hackage/
            secure: True
            key-threshold: 3
            root-keys:
               f76d08be13e9a61a377a85e2fb63f4c5435d40f8feb3e12eb05905edb8cdea89
               26021a13b401500c8eb2761ca95c61f2d625bfef951b939a8124ed12ecf07329
               7541f32a4ccca4f97aea3b22f5e593ba2c0267546016b992dfadcd2fe944e55d
            --sha256: sha256-Bkn2Etb0JVmb7tM7jxuIoYLFnSp7acqraEYVq0I5oUM=

          active-repositories: hackage.haskell.org, head.hackage.ghc.haskell.org:override
        '';
      } // final.lib.optionalAttrs (__compareVersions final.buildPackages.haskell-nix.compiler.${compiler-nix-name}.version "9.8.0" < 0) {
        index-state = final.haskell-nix.internalHackageIndexState;
        materialized = ../materialized + "/${compiler-nix-name}/cabal-install";
      } // args));

    # Memoize the cabal-install and nix-tools derivations by adding:
    #   haskell-nix.cabal-install.ghcXXX
    #   haskell-nix.cabal-install-unchecked.ghcXXX
    #   haskell-nix.nix-tools.ghcXXX
    #   haskell-nix.nix-tools-unchecked.ghcXXX
    # Using these avoids unnecessary calls to mkDerivation.
    # For cabal projects we match the versions used to the compiler
    # selected for the project to avoid the chance of a dependency
    # another GHC version (particularly useful on macOS where
    # executables are dynamically linked to GHC itself, which means
    # that if you use a tool built with a different GHC you will get
    # that GHC itself in your closure).
    cabal-install = final.lib.mapAttrs (compiler-nix-name: _:
      final.haskell-nix.cabal-install-tool { inherit compiler-nix-name; }) final.haskell-nix.compiler;
    cabal-install-unchecked = final.lib.mapAttrs (compiler-nix-name: _:
      final.haskell-nix.cabal-install-tool {
        compiler-nix-name =
          # If there is no materialized version for this GHC version fall back on
          # a version of GHC for which there will be.
          if builtins.pathExists (../materialized + "/${compiler-nix-name}/cabal-install/default.nix")
            then compiler-nix-name
            else "ghc928";
        checkMaterialization = false;
      }) final.haskell-nix.compiler;

    # These `internal` versions are used for:
    # * `nix-tools` for stack projects (since we use `nix-tools` to process
    #   the `stack.yaml` file we cannot match the ghc of the project the
    #   way we do for cabal projects).
    # * Scripts are used to update stackage and hackage
    # Updating the version of GHC selected here should be fairly safe as
    # there should be no difference in the behaviour of these tools.
    # (stack projects on macOS may see a significant change in the
    # closure size of their build dependencies due to dynamic linking).
    internal-cabal-install =
      final.haskell-nix.cabal-install-tool {
        compiler-nix-name = "ghc8107";
        compilerSelection = p: p.haskell.compiler;
        checkMaterialization = false;
      };

    # WARN: The `import ../. {}` will prevent
    #       any cross to work, as we will loose
    #       the `config` value.
    # As such the following sadly won't work :(
    # haskellPackages = with import ../. {}; {
    #     hpack = null;
    #     hello = (hackage-package {
    #         inherit (final) cabal-install;
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

    # the bootstrap infrastructure (pre-compiled ghc; bootstrapped cabal-install, ...)
    bootstrap =
      let
        # This compiler-nix-name will only be used to build nix-tools and cabal-install
        # when checking materialization of alex, happy and hscolour.
        compiler-nix-name = buildBootstrapper.compilerNixName;
        # The ghc boot compiler to use to compile alex, happy and hscolour
        ghcOverride = final.buildPackages.haskell-nix.bootstrap.compiler.${compiler-nix-name};
        index-state = final.haskell-nix.internalHackageIndexState;
      in {
        compiler = final.haskell.compiler;
        packages = {
            # now that we have nix-tools and hpack, we can just
            # use `hackage-package` to build any package from
            # hackage with haskell.nix.  For alex and happy we
            # need to use the boot strap compiler as we need them
            # to build ghcs from source.
            # guardMaterializationChecks is used here so we
            # can turn off materialization checks when
            # building ghc itself (since GHC is a dependency
            # of the materialization check it would cause
            # infinite recursion).
            alex-tool = args: final.haskell-nix.tool buildBootstrapper.compilerNixName "alex" ({config, pkgs, ...}: {
                compilerSelection = p: p.haskell.compiler;
                evalPackages = pkgs.buildPackages;
                version = "3.2.4";
                inherit ghcOverride index-state;
                materialized = ../materialized/bootstrap + "/${buildBootstrapper.compilerNixName}/alex";
                modules = [{ reinstallableLibGhc = false; }];
                nix-tools = config.evalPackages.haskell-nix.nix-tools;
            } // args);
            alex = final.haskell-nix.bootstrap.packages.alex-tool {};
            alex-unchecked = final.haskell-nix.bootstrap.packages.alex-tool { checkMaterialization = false; };
            happy-tool = { version ? "1.19.12", ... }@args: final.haskell-nix.tool buildBootstrapper.compilerNixName "happy"
              ({config, pkgs, ...}: {
                compilerSelection = p: p.haskell.compiler;
                evalPackages = pkgs.buildPackages;
                inherit version ghcOverride index-state;
                materialized = ../materialized/bootstrap + "/${buildBootstrapper.compilerNixName}/happy-${version}";
                modules = [{ reinstallableLibGhc = false; }];
                nix-tools = config.evalPackages.haskell-nix.nix-tools;
              } // args);
            happy = final.haskell-nix.bootstrap.packages.happy-tool {};
            happy-unchecked = final.haskell-nix.bootstrap.packages.happy-tool { checkMaterialization = false; };
            # Older version needed when building ghc 8.6.5
            happy-old = final.haskell-nix.bootstrap.packages.happy-tool { version = "1.19.11"; };
            happy-old-unchecked = final.haskell-nix.bootstrap.packages.happy-tool { version = "1.19.11"; checkMaterialization = false; };
            hscolour-tool = args: (final.haskell-nix.hackage-package
              ({config, pkgs, ...}: {
                compilerSelection = p: p.haskell.compiler;
                evalPackages = pkgs.buildPackages;
                compiler-nix-name = buildBootstrapper.compilerNixName;
                name = "hscolour";
                version = "1.24.4";
                inherit ghcOverride index-state;
                materialized = ../materialized/bootstrap + "/${buildBootstrapper.compilerNixName}/hscolour";
                modules = [{ reinstallableLibGhc = false; }];
                nix-tools = config.evalPackages.haskell-nix.nix-tools;
            } // args)).getComponent "exe:HsColour";
            hscolour = final.haskell-nix.bootstrap.packages.hscolour-tool {};
            hscolour-unchecked = final.haskell-nix.bootstrap.packages.hscolour-tool { checkMaterialization = false; };
        };
    };
  };
}
