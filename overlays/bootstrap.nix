final: prev:
let
    buildBootstrapper.compilerNixName = "ghc8107";
    latestVerMap = {
      "8.10" = "8.10.7";
      "9.0" = "9.0.2";
      "9.2" = "9.2.8";
      "9.4" = "9.4.8";
      "9.6" = "9.6.6";
      "9.8" = "9.8.2";
      "9.10" = "9.10.1";
    };
    gitInputs = {
      ghc911 = "9.11";
    };
    versionToNixName = v: "ghc${builtins.replaceStrings ["."] [""] v}";
    compilerNameMap =
      builtins.mapAttrs (source-name: v:
        versionToNixName "${v}.${builtins.substring 0 8 final.haskell-nix.sources.${source-name}.lastModifiedDate}")
          gitInputs //
      builtins.listToAttrs (map (v:
        { name = versionToNixName v; value = versionToNixName latestVerMap.${v}; })
          (builtins.attrNames latestVerMap));
    traceWarnOld = v: x:
      let
        bootstrapGhc = final.buildPackages.haskell-nix.bootstrap.compiler."${buildBootstrapper.compilerNixName}";
      in
      if builtins.compareVersions x.version bootstrapGhc.version < 0 then
          throw "Desired GHC (${x.version}) is older than the bootstrap GHC (${bootstrapGhc.version}) for this platform (${final.stdenv.targetPlatform.config})."
      else x // final.lib.optionalAttrs (x.version != latestVerMap.${v}) { latestVersion = latestVerMap.${v}; };
    errorOldGhcjs = v: up: throw "ghcjs ${v} is no longer supported by haskell.nix. Consider using ${latestVerMap.${up}}";
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
                inherit (final.haskell-nix.bootstrap.packages) alex happy hscolour;
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
            sphinx = final.buildPackages.sphinx;

            ghc-patches = version: let
                # Returns true iff this derivation's version is strictly older than ver.
                versionLessThan = ver: builtins.compareVersions ver version == 1;
                # Returns true iff this derivation's version is greater than or equal to ver.
                versionAtLeast = ver: !versionLessThan ver;
                fromUntil = start: end: final.lib.optional (versionAtLeast start && versionLessThan end);
                from = start: final.lib.optional (versionAtLeast start);
                until = end: final.lib.optional (versionLessThan end);
                always = final.lib.optional true;
                onDarwin = final.lib.optionals final.stdenv.targetPlatform.isDarwin; 
                onMusl = final.lib.optionals final.stdenv.targetPlatform.isMusl;
                onWindows = final.lib.optionals final.stdenv.targetPlatform.isWindows;
                onWindowsOrMusl = final.lib.optionals (final.stdenv.targetPlatform.isWindows || final.stdenv.targetPlatform.isMusl);
                onAarch32 = final.lib.optionals final.stdenv.targetPlatform.isAarch32;
                onAarch64 = final.lib.optionals final.stdenv.targetPlatform.isAarch64;
                onAarch64Musl = final.lib.optionals (final.stdenv.targetPlatform.isAarch64 && final.stdenv.targetPlatform.isMusl);
                onAndroid = final.lib.optionals final.stdenv.targetPlatform.isAndroid;
                onNative = final.lib.optionals (final.stdenv.buildPlatform == final.stdenv.targetPlatform);
                onCross = final.lib.optionals (final.stdenv.targetPlatform != final.stdenv.hostPlatform);
                onGhcjs = final.lib.optionals final.stdenv.targetPlatform.isGhcjs;
                on32bit = final.lib.optionals final.stdenv.targetPlatform.is32bit;
                # Try to avoid reordering the patches unless a patch is added or changed that
                # will be applied to most versions of the GHC anyway (reordering the patches
                # results in rebuilds of GHC and reduces sharing in /nix/store).
                in until             "9.11"  ./patches/ghc/dll-loader-8.4.2.patch                                # https://gitlab.haskell.org/ghc/ghc/merge_requests/949  -- open
                ++ until             "9.2"   ./patches/ghc/ghc-8.4.3-Cabal2201-no-hackage-tests.patch            # ?
                ++ until             "9.2"   ./patches/ghc/cabal-host.patch                                      # https://github.com/haskell/cabal/issues/5887
                ++ fromUntil "9.2"   "9.4"   ./patches/ghc/ghc-9.2-cabal-host.patch                              # https://github.com/haskell/cabal/issues/5887
                ++ until             "9.0"   ./patches/ghc/respect-ar-path.patch
                ++ onDarwin (until             "9.0"   ./patches/ghc/ghc-macOS-loadArchive-fix.patch)
                ++ onDarwin (fromUntil "9.0.0" "9.2"   ./patches/ghc/ghc-9.0-macOS-loadArchive-fix.patch)
                ++ onDarwin (fromUntil "9.2.0" "9.4"   ./patches/ghc/ghc-9.2-macOS-loadArchive-fix.patch)
                ++ onDarwin (until             "9.0.2" ./patches/ghc/ghc-8.10-darwin-gcc-version-fix.patch)

                # See https://github.com/input-output-hk/haskell.nix/issues/1027
                ++ onAarch32 (until             "9.2" ./patches/ghc/ghc-8.10-3434-armv7a.patch)
                ++ onAarch64 (until             "9.2" ./patches/ghc/ghc-8.10-3434.patch)
                ++ onAarch64 (fromUntil "9.2.1" "9.4" ./patches/ghc/ghc-9.2-3434.patch)
                ++ onAndroid (fromUntil "9.2.1" "9.4" ./patches/ghc/ghc-9.2-3434.patch)

                ++ until              "9.4"    ./patches/ghc/ghc-acrt-iob-func.patch
                ++ until              "9.2"    ./patches/ghc/ghc-mprotect-nonzero-len.patch
                ++ fromUntil "9.2"    "9.2.8"  ./patches/ghc/ghc-9.2-fix-m32_allocator_init-10453.patch # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/10453
                ++ fromUntil "9.2"    "9.2.8"  ./patches/ghc/ghc-9.2-fix-m32_allocator_init-10453-2.patch
                ++ fromUntil "9.2"    "9.4"    ./patches/ghc/ghc-9.2-bignum-expose-backendName.patch # https://gitlab.haskell.org/ghc/ghc/-/commit/bc498fdfa482dfe796e3a12ac8f9d71913930740

                ++ until              "9.0"    ./patches/ghc/ghc-8.10.5-ubxt.patch
                ++ final.lib.optionals (!final.stdenv.targetPlatform.isDarwin)
                  (  until              "9.2"    ./patches/ghc/Cabal-3886.patch
                  ++ fromUntil "9.2"    "9.4"    ./patches/ghc/ghc-9.2-Cabal-3886.patch)

                ++ onWindows (until   "9.4"    ./patches/ghc/ghc-8.10-z-drive-fix.patch)
                ++ until              "9.4"    ./patches/ghc/ghc-8.10-windows-add-dependent-file.patch
                ++ until              "9.0"    ./patches/ghc/Cabal-unbreak-GHCJS.patch
                ++ fromUntil "9.0.1"  "9.0.2"  ./patches/ghc/AC_PROG_CC_99.patch
                ++ fromUntil "9.0.2"  "9.2.2"  ./patches/ghc/ghc-9.2.1-xattr-fix.patch      # Problem was backported to 9.0.2
                ++ until              "9.4"    ./patches/ghc/MR6654-nonmoving-maxmem.patch  # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6654
                ++ until              "9.0.2"  ./patches/ghc/MR6617-nonmoving-mvar.patch    # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6617
                ++ until              "9.0.2"  ./patches/ghc/MR6595-nonmoving-mutvar.patch  # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6595
                ++ until              "9.2"    ./patches/ghc/ghc-8.10-global-unique-counters-in-rts.patch # backport of https://gitlab.haskell.org/ghc/ghc/-/commit/9a28680d2e23e7b25dd7254a439aea31dfae32d5
                ++ fromUntil "9.2"    "9.4"    ./patches/ghc/ghc-9.2-global-unique-counters-in-rts.patch # backport of https://gitlab.haskell.org/ghc/ghc/-/commit/9a28680d2e23e7b25dd7254a439aea31dfae32d5
                ++ until              "9.2"    ./patches/ghc/issue-18708.patch              # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/6554
                ++ fromUntil "9.2.2"  "9.4"    ./patches/ghc/ghc-9.2.2-fix-warnings-building-with-self.patch # https://gitlab.haskell.org/ghc/ghc/-/commit/c41c478eb9003eaa9fc8081a0039652448124f5d
                ++ until              "9.6"    ./patches/ghc/ghc-hpc-response-files.patch   # https://gitlab.haskell.org/ghc/ghc/-/merge_requests/8194
                ++ fromUntil "9.2"    "9.12"   ./patches/ghc/sanity-check-find-file-name.patch
                ++ until              "9.0"    ./patches/ghc/dont-mark-evacuate_large-as-inline.patch
                ++ onWindows (fromUntil "9.4.1"  "9.4.5"  ./patches/ghc/ghc-9.4-hadrian-win-cross.patch)
                ++ onWindows (fromUntil "9.4.7"  "9.4.9"  ./patches/ghc/ghc-9.8-hadrian-win-cross.patch)
                ++ onWindows (fromUntil "9.6.3"  "9.11"   ./patches/ghc/ghc-9.8-hadrian-win-cross.patch)
                # support R_X86_64_PC64 (ELF constant 24) - IMAGE_REL_AMD64_SREL32 (PE constant 14), which seems to appear with 9.6 more frequently, and
                # results in "unhandled PEi386 relocation type 14".
                ++ onWindows (fromUntil "9.4.1"  "9.11"   ./patches/ghc/win-reloc-x86_64-pc64.patch)
                # ++ onWindows (fromUntil "9.4.1"  "9.10"   ./patches/ghc/Win32-depends-on-mingwex.patch)
                # if the host system provides ucrt (e.g. wine with ucrtbase.dll), we may end up linking against symbols from ucrtbase, instead of msvcrt,
                # thus leading to broken code.  E.g. the handles we create and hand to wine will all be busted, because they come from one and are processed
                # by another crt.
                ++ final.lib.optionals (final.stdenv.targetPlatform.libc != "ucrt") (
                    onWindows (until               "9.8"   ./patches/ghc/win-linker-no-ucrt.patch)
                  # Nixos/nixpkgs is mscvrt for now, thus we must disable ucrt in ghc, otherwise we end up with broken linking.
                  ++ onWindows (fromUntil "9.4.1"  "9.6"    ./patches/ghc/no-ucrt-9.4.patch)
                  ++ onWindows (fromUntil "9.6.1"  "9.6.3"  ./patches/ghc/no-ucrt-9.6.patch)
                  ++ onWindows (fromUntil "9.6.3"  "9.8"    ./patches/ghc/no-ucrt-9.6.3.patch)
                  ++ onWindows (fromUntil "9.8"    "9.9"    ./patches/ghc/no-ucrt-9.8.patch)
                  ++ onWindows (fromUntil "9.9"    "9.9.20231203" ./patches/ghc/no-ucrt-9.9.patch)
                )
                ++ onWindows (fromUntil "9.4.7"  "9.5"    ./patches/ghc/revert-289547580b6f2808ee123f106c3118b716486d5b.patch)
                ++ onWindows (fromUntil "9.6.3"  "9.11"   ./patches/ghc/revert-289547580b6f2808ee123f106c3118b716486d5b.patch)
                # the following is needed for cardano-prelude as it uses closure_sizeW :-/
                ++ onWindows (fromUntil "9.4"    "9.11"   ./patches/ghc/win-add-closure_sizeW-to-rtssyms.patch)
                ++ onWindows (until              "9.0"    ./patches/ghc/ghc-8.10-win-add-tzset-to-rtssyms.patch)
                ++ onWindows (fromUntil "9.0"    "9.3"    ./patches/ghc/ghc-9.2-win-add-tzset-to-rtssyms.patch)
                ++ onWindows (fromUntil "9.4"    "9.11"   ./patches/ghc/win-add-tzset-to-rtssyms.patch)
                ++ onWindows (fromUntil "9.4.1"  "9.4.7"  ./patches/ghc/win-linker-no-null-deref.patch)
                ++ onWindows (fromUntil "9.4.7"  "9.4.8"  ./patches/ghc/win-linker-no-null-deref-9.6.patch)
                ++ onWindows (fromUntil "9.4.1"  "9.6"    ./patches/ghc/ghc-9.4-drop-mingwex-from-base.patch)
                ++ onWindows (fromUntil "9.6.1"  "9.6.3"  ./patches/ghc/win-linker-no-null-deref.patch)
                ++ onWindows (fromUntil "9.6.3"  "9.6.4"  ./patches/ghc/win-linker-no-null-deref-9.6.patch)
                ++ onWindows (fromUntil "9.8.1"  "9.8.2"  ./patches/ghc/win-linker-no-null-deref-9.6.patch)
                ++ onWindows (until              "9.0"    ./patches/ghc/ghc-8.10-windres-invocation.patch)
                ++ onWindows (fromUntil "9.0"    "9.4"    ./patches/ghc/ghc-9.0-windres-invocation.patch)
                ++ fromUntil "9.4.5"  "9.4.9"         ./patches/ghc/ghc-9.4.5-include-order-fix.patch
                ++ fromUntil "9.6.2"  "9.8"           ./patches/ghc/ghc-9.4.5-include-order-fix.patch
                ++ fromUntil "9.6.1"  "9.9.20231203"  ./patches/ghc/MR10116.patch
                ++ onNative (fromUntil "9.4.1" "9.6"   ./patches/ghc/hadrian-build-deriveConstants-genprimopcode-ghc94.patch)
                ++ onNative (fromUntil "9.6.1" "9.12"  ./patches/ghc/hadrian-build-deriveConstants-genprimopcode.patch)
                ++ onGhcjs (fromUntil "9.6.1" "9.6.3" ./patches/ghc/ghc-9.6-Merge-libiserv-with-ghci.patch)
                ++ onGhcjs (fromUntil "9.6.3" "9.8"   ./patches/ghc/ghc-9.6.3-Merge-libiserv-with-ghci.patch)
                ++ onGhcjs (fromUntil "9.6.1" "9.8"   ./patches/ghc/ghc-9.6-Assorted-changes-to-avoid-head-tail.patch)
                ++ onGhcjs (fromUntil "9.6.1" "9.6.3" ./patches/ghc/ghc-9.6-JS-implement-TH-support.patch)
                ++ onGhcjs (fromUntil "9.6.3" "9.8"   ./patches/ghc/ghc-9.6.3-JS-implement-TH-support.patch)
                ++ fromUntil "9.8.1"  "9.8.2"  ./patches/ghc/ghc-9.8-cabal-c-soures-fix.patch
                ++ fromUntil "9.6.3"  "9.6.5"  ./patches/ghc/ghc-9.6.3-Cabal-9384.patch
                ++ fromUntil "9.8.1"  "9.9"    ./patches/ghc/ghc-9.6.3-Cabal-9384.patch

                # the following is a partial reversal of https://gitlab.haskell.org/ghc/ghc/-/merge_requests/4391, to address haskell.nix#1227
                ++ onAarch64 (until "9.0" ./patches/ghc/mmap-next.patch)
                ++ onAarch64 (until "9.0" ./patches/ghc/m32_alloc.patch)
                ++ onAndroid (until "9.0" ./patches/ghc/rts-android-jemalloc-qemu.patch)
                ++ onAndroid (until "9.0" ./patches/ghc/stack-protector-symbols.patch)
                ++ onAndroid (until "9.0" ./patches/ghc/libraries-prim-os-android.patch)
                ++ onAndroid (until "9.0" ./patches/ghc/ghc-rts-linker-condbr.patch)
                # due to mmap-next renaming we need different ones for aarch64 and aarch32 m(
                ++ onAndroid (onAarch32 (until "9.0" ./patches/ghc/ghc-8.10.7-linker-weak-and-common-armv7a.patch))
                ++ onAndroid (onAarch64 (until "9.0" ./patches/ghc/ghc-8.10.7-linker-weak-and-common.patch))
                ++ onAndroid (onAarch32 (until "9.0" ./patches/ghc/libc-memory-symbols-armv7a.patch))
                ++ onAndroid (onAarch64 (until "9.0" ./patches/ghc/libc-memory-symbols.patch))
                ++ onAndroid (until "9.0" ./patches/ghc/android-base-needs-iconv.patch)
                ++ onCross   (until "9.4" ./patches/ghc/ghc-make-stage-1-lib-ghc.patch)
                ++ onAarch64 (until "9.0" ./patches/ghc/ghc-8.10-better-symbol-addr-debug.patch)
                ++ onAarch64 (until "9.0" ./patches/ghc/ghc-8.10-aarch64-handle-none-rela.patch)
                ++ onWindows (until "9.0" ./patches/ghc/5b08e0c06e038448a63aa9bd7f163b23d824ba4b.patch)
                ++ onAarch64 (fromUntil "9.0" "9.11" ./patches/ghc/ghc-9.0-better-symbol-addr-debug.patch)
                ++ onAarch64 (fromUntil "9.0" "9.11" ./patches/ghc/ghc-9.0-aarch64-handle-none-rela.patch)

                ++ onWindows (fromUntil "9.6.3" "9.6.4" ./patches/ghc/ghc-9.6-hadrian-splitsections.patch)
                ++ onWindows (fromUntil "9.8.1" "9.8.2" ./patches/ghc/ghc-9.6-hadrian-splitsections.patch)

                # this patch was backported to 9.4.8 and 9.6.4
                ++ onWindows (fromUntil "9.4" "9.4.8" ./patches/ghc/ghc-9.6-fix-code-symbol-jumps.patch)
                # we also want to apply this to musl, so that the ./patches/ghc/ghc-9.6-0006-Adds-support-for-Hidden-symbols.patch applies.
                ++ onWindowsOrMusl (fromUntil "9.6" "9.6.4" ./patches/ghc/ghc-9.6-fix-code-symbol-jumps.patch)
                ++ onWindows (fromUntil "9.8" "9.8.2" ./patches/ghc/ghc-9.6-fix-code-symbol-jumps.patch)
                # this one is to allow linking extra symbols from iserv.
                # ++ fromUntil "9.6.1" "9.10"                                                                                       ./patches/ghc/iserv-syms.patch
                # Fix the bad fixups: https://gitlab.haskell.org/ghc/ghc/-/commit/2adc050857a9c1b992040fbfd55fbe65b2851b19
                ++ onAarch64 (fromUntil "9.6" "9.6.4" ./patches/ghc/2adc050857a9c1b992040fbfd55fbe65b2851b19.patch)

                ++ final.lib.optionals (
                        final.stdenv.targetPlatform.isAarch64
                     && final.stdenv.targetPlatform.isMusl
                     && final.stdenv.targetPlatform != final.stdenv.hostPlatform)
                  (until "9.0" ./patches/ghc/ghc-8.10-aarch64-musl-gettimeofday.patch)

                # This one will lead to segv's on darwin, when calling `strlen` during lookupStrHashTable. `strlen` ends up being called with 0x0.
                # This patch will allow adding additional symbols to iserv, instead of having to patch them into GHC all the time.
                ++ final.lib.optionals (
                        (final.stdenv.targetPlatform.isAndroid || final.stdenv.targetPlatform.isLinux)
                     && (final.stdenv.targetPlatform.isAarch64 || final.stdenv.targetPlatform.is32bit))
                  (fromUntil "9.6.1" "9.11" ./patches/ghc/iserv-syms.patch)
                ++ onAndroid (until "9.0" ./patches/ghc/ghc-8.10.7-weak-symbols-2.patch)
                ++ onDarwin (onAarch64 (until "9.0" ./patches/ghc/ghc-8.10.7-rts-aarch64-darwin.patch))
                ++ onAndroid (onAarch32 (until "9.2" ./patches/ghc/ghc-8.10-android.patch))
                ++ onAndroid (onAarch32 (until "9.2" ./patches/ghc/ghc-8.10.7-android-bionic-symbols.patch))
                ++ onAndroid (onAarch32 (until "9.2" ./patches/ghc/ghc-8.10.7-bionic-libc.patch))
                ++ onAndroid (onAarch32 (until "9.2" ./patches/ghc/ghc-8.10.7-cross-dont-build-stage2-tools.patch))
                ++ onAndroid (fromUntil "9.0" "9.8"./patches/ghc/ghc-9.6-hadrian-android.patch)
                ++ onMusl (onAarch64 (fromUntil "9.4"  "9.8"  ./patches/ghc/ghc-9.6-hadrian-strip-cmd.patch))
                ++ onMusl (onAarch64 (fromUntil "9.8"  "9.10" ./patches/ghc/ghc-9.8-hadrian-strip-cmd.patch))
                ++ onMusl (onAarch64 (fromUntil "9.10" "9.12" ./patches/ghc/ghc-9.10-hadrian-strip-cmd.patch))
                ++ on32bit (fromUntil "9.0" "9.4" ./patches/ghc/ghc-9.6-32bit-cmm.patch)
                ++ onAndroid (fromUntil "9.6.3" "9.10" ./patches/ghc/ghc-9.6-iog.patch)
                ++ onAndroid (fromUntil "9.6" "9.9" ./patches/ghc/ghc-9.6-debug-secno.patch)
                ++ onAndroid (on32bit (fromUntil "9.6" "9.9"./patches/ghc/ghc-9.6-missing-symbols-deadbeef.patch))

                # Allow loading static external plugins into cross compilers
                ++ onCross (fromUntil "9.6.1" "9.11" ./patches/ghc/5c80a27488acfe3610ddfcb99a1e961002e386d0.patch)
                ++ onCross (fromUntil "9.6.1" "9.11" ./patches/ghc/f8beb54a1d5725bd0d8a4b0a909d1b41d742b50b.patch)
                ++ final.lib.optionals (
                        final.stdenv.targetPlatform.isAndroid
                     && final.stdenv.targetPlatform.is32bit
                     || final.stdenv.targetPlatform.isMusl)
                  (until "9.11" ./patches/ghc/ghc-9.6-missing-symbols-deadbeef.patch)
                ++ onAarch64Musl (fromUntil "9.6" "9.11" ./patches/ghc/ghc-9.6-linker-pool-allocator.patch)
                ++ onAarch64Musl (fromUntil "9.6" "9.11" ./patches/ghc/ghc-9.6-linker-pool-allocator-2.patch)

                ++ onMusl (fromUntil "9.6" "9.8" ./patches/ghc/ghc-9.6-0001-Refactor-IServ.hs.patch)
                ++ onMusl (fromUntil "9.6" "9.11" ./patches/ghc/ghc-9.6-0002-Drop-spurious-8-byte-offset-from-elf_plt.patch)
                ++ onAarch64Musl (fromUntil "9.6" "9.11" ./patches/ghc/ghc-9.6-0003-Better-pool-alignment.-We-still-hardcode-section-ali.patch)
                ++ onAarch64Musl (fromUntil "9.6" "9.11" ./patches/ghc/ghc-9.6-0007-fixup-Better-pool-alignment.-We-still-hardcode-secti.patch)
                ++ onAarch64Musl (fromUntil "9.6" "9.11" ./patches/ghc/ghc-9.6-0008-pool-improvements.patch)
                # these two are abit questionable. They are pretty rough, and assume static binary as well as posix.
                # onMusl (fromUntil "9.6" "9.11" ./patches/ghc/ghc-9.6-0004-ghcidladdr.patch)
                # onMusl (fromUntil "9.6" "9.11" ./patches/ghc/ghc-9.6-0005-Better-interpreter-debugging.-Needs-ghcidladdr.patch)

                # Fix docs/users_guide/rtd-theme/layout.html to work with sphinx 7
                ++ fromUntil "9.0" "9.8" ./patches/ghc/docs-sphinx-7.patch
                ++ fromUntil "9.8" "9.9" ./patches/ghc/docs-sphinx-7-ghc98.patch

                # These two patches are needed for libblst, which has now hidden symbols, which the linker doesn't know how to deal with.
                ++ until "9.0" ./patches/ghc/ghc-8.10-0006-Adds-support-for-Hidden-symbols.patch
                ++ until "9.0" ./patches/ghc/ghc-8.10-0006-Adds-support-for-Hidden-symbols-2.patch
                ++ onWindowsOrMusl (fromUntil "9.6"    "9.7"  ./patches/ghc/ghc-9.6-0006-Adds-support-for-Hidden-symbols.patch)
                ++ onWindowsOrMusl (fromUntil "9.8.2"  "9.11" ./patches/ghc/ghc-9.6-0006-Adds-support-for-Hidden-symbols.patch)
                ++ onWindowsOrMusl (fromUntil "9.6"    "9.7"  ./patches/ghc/ghc-9.6-0006-Adds-support-for-Hidden-symbols-2.patch)
                ++ onWindowsOrMusl (fromUntil "9.8.2"  "9.11" ./patches/ghc/ghc-9.6-0006-Adds-support-for-Hidden-symbols-2.patch)
                ++ fromUntil "9.9"  "9.12" ./patches/ghc/ghc-9.9-Cabal-3.11.patch
                ++ fromUntil "9.8"  "9.9"  ./patches/ghc/ghc-9.8-text-upper-bound.patch
                ++ fromUntil "9.10" "9.11" ./patches/ghc/ghc-9.10-containers-upper-bound.patch
                ++ fromUntil "9.10" "9.12" ./patches/ghc/ghc-9.10-merge-objects.patch

                # This patch will make windows stop emitting absolute relocations. This is one way in which binutils 2.36+ (with ASLR enabled), will just choke on the
                # assembly we generate because it's always absolute (32bit) addressing modes.
                # GHC from 9.6+ seems to have https://gitlab.haskell.org/ghc/ghc/-/merge_requests/7449, which should fix this as well.
                ++ onWindows (until "9.0" ./patches/ghc/windows-pseudo-pic-8.10.patch)
                ++ onWindows (fromUntil "9.0" "9.2" ./patches/ghc/windows-pseudo-pic.patch)
                ++ onWindows (fromUntil "9.2" "9.4" ./patches/ghc/windows-pseudo-pic-9.2.patch)

                # Fix issue loading windows dll using `.dll.a` file
                ++ onWindows (fromUntil "9.4" "9.12" ./patches/ghc/ghc-9.10-windows-dll-dependent-symbol-type-fix.patch)
                ;
        in ({
            ghc8107 = traceWarnOld "8.10" (final.callPackage ../compiler/ghc {
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

                src-spec.file = final.haskell-nix.sources.ghc8107;
                src-spec.version = "8.10.7";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "8.10.7";
            });
            ghc901 = traceWarnOld "9.0" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc901; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc901
                    else final.buildPackages.buildPackages.haskell.compiler.ghc902;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc901;
                src-spec.version = "9.0.1";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.0.1";
            });
            ghc902 = traceWarnOld "9.0" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc902; };

                bootPkgs = bootPkgs // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc902
                    else final.buildPackages.buildPackages.haskell.compiler.ghc902;
                };
                inherit sphinx;

                useLLVM = !final.stdenv.targetPlatform.isx86;
                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc902;
                src-spec.version = "9.0.2";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.0.2";
            });
            ghc921 = traceWarnOld "9.2" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc921; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc921;
                src-spec.version = "9.2.1";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.2.1";
            });
            ghc922 = traceWarnOld "9.2" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc922; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc922;
                src-spec.version = "9.2.2";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.2.2";
            });
            ghc923 = traceWarnOld "9.2" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc923; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc923;
                src-spec.version = "9.2.3";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.2.3";
            });
            ghc924 = traceWarnOld "9.2" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc924; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc924;
                src-spec.version = "9.2.4";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.2.4";
            });
            ghc925 = traceWarnOld "9.2" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc925; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc925;
                src-spec.version = "9.2.5";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.2.5";
            });
            ghc926 = traceWarnOld "9.2" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc926; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc926;
                src-spec.version = "9.2.6";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.2.6";
            });
            ghc927 = traceWarnOld "9.2" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc927; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc927;
                src-spec.version = "9.2.7";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.2.7";
            });
            ghc928 = traceWarnOld "9.2" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc928; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = final.buildPackages.buildPackages.haskell-nix.compiler.ghc8107;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc928;
                src-spec.version = "9.2.8";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.2.8";
            });
            ghc941 = traceWarnOld "9.4" (final.callPackage ../compiler/ghc {
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

                src-spec.file = final.haskell-nix.sources.ghc941;
                src-spec.version = "9.4.1";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.4.1";
            });
            ghc942 = traceWarnOld "9.4" (final.callPackage ../compiler/ghc {
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

                src-spec.file = final.haskell-nix.sources.ghc942;
                src-spec.version = "9.4.2";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.4.2";
            });
            ghc943 = traceWarnOld "9.4" (final.callPackage ../compiler/ghc {
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

                src-spec.file = final.haskell-nix.sources.ghc943;
                src-spec.version = "9.4.3";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.4.3";
            });
            ghc944 = traceWarnOld "9.4" (final.callPackage ../compiler/ghc {
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

                src-spec.file = final.haskell-nix.sources.ghc944;
                src-spec.version = "9.4.4";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.4.4";
            });
            ghc945 = traceWarnOld "9.4" (final.callPackage ../compiler/ghc {
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

                src-spec.file = final.haskell-nix.sources.ghc945;
                src-spec.version = "9.4.5";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.4.5";
            });
            ghc947 = traceWarnOld "9.4" (final.callPackage ../compiler/ghc {
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

                src-spec.file = final.haskell-nix.sources.ghc947;
                src-spec.version = "9.4.7";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.4.7";
            });
            ghc948 = traceWarnOld "9.4" (final.callPackage ../compiler/ghc {
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

                src-spec.file = final.haskell-nix.sources.ghc948;
                src-spec.version = "9.4.8";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.4.8";
            });
            ghc961 = traceWarnOld "9.6" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc961; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc961
                    else final.buildPackages.buildPackages.haskell.compiler.ghc966
                          or final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc961;
                src-spec.version = "9.6.1";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.6.1";
            });
            ghc962 = traceWarnOld "9.6" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc962; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc962
                    else final.buildPackages.buildPackages.haskell.compiler.ghc966
                          or final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc962;
                src-spec.version = "9.6.2";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.6.2";
            });
            ghc963 = traceWarnOld "9.6" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc963; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc963
                    else final.buildPackages.buildPackages.haskell.compiler.ghc966
                          or final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc963;
                src-spec.version = "9.6.3";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.6.3";
            });
            ghc964 = traceWarnOld "9.6" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc964; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc964
                    else final.buildPackages.buildPackages.haskell.compiler.ghc966
                          or final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc964;
                src-spec.version = "9.6.4";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.6.4";
            });
            ghc965 = traceWarnOld "9.6" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc965; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc965
                    else final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc965;
                src-spec.version = "9.6.5";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.6.5";
            });
            ghc966 = traceWarnOld "9.6" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc966; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc966
                    else final.buildPackages.buildPackages.haskell.compiler.ghc966
                          or final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc966;
                src-spec.version = "9.6.6";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.6.6";
            });
            ghc981 = traceWarnOld "9.8" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc981; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc966
                    else final.buildPackages.buildPackages.haskell.compiler.ghc966
                          or final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc981;
                src-spec.version = "9.8.1";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.8.1";
            });
            ghc982 = traceWarnOld "9.8" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc982; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc966
                    else final.buildPackages.buildPackages.haskell.compiler.ghc966
                          or final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_12;
                llvmPackages = final.llvmPackages_12;

                src-spec.file = final.haskell-nix.sources.ghc982;
                src-spec.version = "9.8.2";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.8.2";
            });
            ghc9101 = traceWarnOld "9.10" (final.callPackage ../compiler/ghc {
                extra-passthru = { buildGHC = final.buildPackages.haskell-nix.compiler.ghc9101; };

                bootPkgs = bootPkgsGhc94 // {
                  ghc = if final.stdenv.buildPlatform != final.stdenv.targetPlatform
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc9101
                    else final.buildPackages.buildPackages.haskell.compiler.ghc982
                          or final.buildPackages.buildPackages.haskell.compiler.ghc981
                          or final.buildPackages.buildPackages.haskell.compiler.ghc966
                          or final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_15;
                llvmPackages = final.llvmPackages_15;

                src-spec.file = final.haskell-nix.sources.ghc9101;
                src-spec.version = "9.10.1";
                src-spec.needsBooting = true;

                ghc-patches = ghc-patches "9.10.1";
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
                    then final.buildPackages.buildPackages.haskell-nix.compiler.ghc9101 # TODO use ${compiler-nix-name}
                    else final.buildPackages.buildPackages.haskell.compiler.ghc982
                          or final.buildPackages.buildPackages.haskell.compiler.ghc981
                          or final.buildPackages.buildPackages.haskell.compiler.ghc966
                          or final.buildPackages.buildPackages.haskell.compiler.ghc965
                          or final.buildPackages.buildPackages.haskell.compiler.ghc964
                          or final.buildPackages.buildPackages.haskell.compiler.ghc963
                          or final.buildPackages.buildPackages.haskell.compiler.ghc962
                          or final.buildPackages.buildPackages.haskell.compiler.ghc945
                          or final.buildPackages.buildPackages.haskell.compiler.ghc944
                          or final.buildPackages.buildPackages.haskell.compiler.ghc943;
                };
                inherit sphinx;

                buildLlvmPackages = final.buildPackages.llvmPackages_15;
                llvmPackages = final.llvmPackages_15;

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
            ghc901 = throw "ghcjs 9.0.1 is not yet supported by haskell.nix";
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
                    raw-src = _evalPackages: booted-ghcjs.configured-src;
                    inherit booted-ghcjs buildGHC;
                    extraConfigureFlags = [
                        "--ghcjs"
                        "--with-ghcjs=${targetPrefix}ghc" "--with-ghcjs-pkg=${targetPrefix}ghc-pkg"
                        "--with-gcc=${final.pkgsBuildBuild.emscripten}/bin/emcc"
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
        version = "3.10.3.0";
      } // args));

    # Memoize the cabal-install and nix-tools derivations by adding:
    #   haskell-nix.cabal-install.ghcXXX
    # Using these avoids unnecessary calls to mkDerivation.
    # For cabal projects we match the versions used to the compiler
    # selected for the project to avoid the chance of a dependency
    # another GHC version (particularly useful on macOS where
    # executables are dynamically linked to GHC itself, which means
    # that if you use a tool built with a different GHC you will get
    # that GHC itself in your closure).
    cabal-install = final.lib.mapAttrs (compiler-nix-name: _:
      final.haskell-nix.cabal-install-tool { inherit compiler-nix-name; }) final.haskell-nix.compiler;

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
            alex = final.haskell-nix.tool buildBootstrapper.compilerNixName "alex" ({config, pkgs, ...}: {
                compilerSelection = p: p.haskell.compiler;
                version = "3.2.4";
                inherit ghcOverride index-state;
                materialized = ../materialized/bootstrap + "/${buildBootstrapper.compilerNixName}/alex";
            });
            happy = final.haskell-nix.tool buildBootstrapper.compilerNixName "happy"
              ({config, pkgs, ...}: {
                compilerSelection = p: p.haskell.compiler;
                version = "1.19.12";
                inherit ghcOverride index-state;
                materialized = ../materialized/bootstrap + "/${buildBootstrapper.compilerNixName}/happy-1.19.12";
              });
            hscolour = (final.haskell-nix.hackage-package
              ({config, pkgs, ...}: {
                compilerSelection = p: p.haskell.compiler;
                compiler-nix-name = buildBootstrapper.compilerNixName;
                name = "hscolour";
                version = "1.24.4";
                inherit ghcOverride index-state;
                materialized = ../materialized/bootstrap + "/${buildBootstrapper.compilerNixName}/hscolour";
            })).getComponent "exe:HsColour";
        };
    };
  };
}
