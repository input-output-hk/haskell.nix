# Build the eval-time `dummy-ghc` script cabal-install runs against
# during plan-to-nix.  Its `--info` output shapes the elaborated
# install plan (and therefore each unit's `pkgHashConfigInputs` and
# UnitId).  Mirrors what real haskell.nix cross-GHCs report so plan-
# nix's recorded UnitIds line up with what the slice's `cabal v2-build`
# computes against the real compiler.
#
# Tested by `test/dummy-ghc-info/default.nix`, which diffs this
# script's `--info` against the real GHC's `--info` (after stripping
# nix-store paths and a small set of known-OK fields).
{ pkgs, evalPackages, ghc }:
let
  # Real GHC normalises a few fields in its platform strings
  # (`Target platform`, `target platform string`) away from the
  # nixpkgs `parsed.*` values:
  #
  #   * 32-bit x86 cpu reported as `i386`, not `i686`
  #     (musl32 / aarch32-style 32-bit cross).
  #   * Windows targets reported as `<cpu>-unknown-mingw32`, not
  #     nixpkgs' `<cpu>-w64-windows` (ucrt64 / mingwW64 cross).
  #
  # Mirror those normalisations here so the dummy and real `--info`
  # outputs match — verified by `tests.dummy-ghc-info` on
  # musl32 (i686) and ucrt64 (x86_64-w64-mingw32).
  platformString = p: with p.parsed;
    let
      cpuName = if cpu.name == "i686" then "i386" else cpu.name;
      vendorName = if p.isWindows then "unknown" else vendor.name;
      kernelName = if p.isWindows then "mingw32" else kernel.name;
    in
      "${cpuName}-${vendorName}-${kernelName}";
in evalPackages.writeTextFile {
  name = "dummy-" + ghc.name;
  executable = true;
  destination = "/bin/${ghc.targetPrefix}ghc";
  # New versions of cabal pass `-package-env=-`, but dummy-ghc can safely ignore it.
  text = ''
    #!${evalPackages.runtimeShell}
    if [[ "$1" == "-package-env=-" ]]; then
      shift
    fi
    case "$*" in
      --version*)
        echo "The Glorious Glasgow Haskell Compilation System, version ${ghc.version}"
        ;;
      --numeric-version*)
        echo "${ghc.version}"
        ;;
    ${pkgs.lib.optionalString (ghc.targetPrefix == "js-unknown-ghcjs-") ''
      --numeric-ghc-version*)
        echo "${ghc.version}"
        ;;
      --numeric-ghcjs-version*)
        echo "${ghc.version}"
        ;;
    ''}
      --supported-languages*)
        cat ${import ./supported-languages.nix { inherit pkgs evalPackages ghc; }}
        ;;
      --print-global-package-db*)
        echo "$out/dumby-db"
        ;;
      --info*)
        echo '[("target os","${
            if pkgs.stdenv.targetPlatform.isLinux
              then "OSLinux"
            else if pkgs.stdenv.targetPlatform.isDarwin
              then "OSDarwin"
            else if pkgs.stdenv.targetPlatform.isWindows
              then "OSMinGW32"
            else if pkgs.stdenv.targetPlatform.isGhcjs
              then "OSGhcjs"
            else if pkgs.stdenv.targetPlatform.isWasi
              then "OSWasi"
            else throw "Unknown target os ${pkgs.stdenv.targetPlatform.config}"
          }")'
        echo ',("target arch","${
            if pkgs.stdenv.targetPlatform.isx86_64
              then "ArchX86_64"
            else if pkgs.stdenv.targetPlatform.isx86
              then "ArchX86"
            else if pkgs.stdenv.targetPlatform.isRiscV64
              then "ArchRISCV64"
            else if pkgs.stdenv.targetPlatform.isAarch64
              then "ArchAArch64"
            else if pkgs.stdenv.targetPlatform.isAarch32
              then "ArchAArch32"
            else if pkgs.stdenv.targetPlatform.isJavaScript
              then "ArchJavaScript"
            else if pkgs.stdenv.targetPlatform.isWasm
              then "ArchWasm32"
            else throw "Unknown target arch ${pkgs.stdenv.targetPlatform.config}"
        }")'
        echo ',("target platform string","${platformString pkgs.stdenv.targetPlatform}")'
        # Real cross GHC reports `Host platform = buildPlatform`
        # (where ghc itself runs), NOT the cross-target platform.
        # `pkgs.stdenv.hostPlatform` for a cross-pkgs context IS the
        # cross target — which would be wrong here.  Use buildPlatform
        # for both Build and Host so dummy mirrors real GHC; the only
        # cross-relevant field is then `Target platform`.
        echo ',("Build platform","${platformString pkgs.stdenv.buildPlatform}")'
        echo ',("Host platform","${platformString pkgs.stdenv.buildPlatform}")'
        echo ',("Target platform","${platformString pkgs.stdenv.targetPlatform}")'
        # `cross compiling` is YES iff GHC's build platform differs
        # from its target platform.  Real GHC emits this on every
        # `--info`; cabal-install reads it to drive cross-toolchain
        # detection.
        #
        # GHC keys "cross" off cpu + kernel, NOT the full triple — a
        # libc switch (e.g. pkgsCross.musl64: gnu → musl on x86_64-
        # linux) reports `cross compiling: NO` because the cpu and
        # kernel both match the build host.  Comparing full
        # `.config` strings would wrongly mark these as cross.
        echo ',("cross compiling","${
          if pkgs.stdenv.buildPlatform.parsed.cpu.name != pkgs.stdenv.targetPlatform.parsed.cpu.name
          || pkgs.stdenv.buildPlatform.parsed.kernel.name != pkgs.stdenv.targetPlatform.parsed.kernel.name
            then "YES" else "NO"}")'
        ${
          # GHC < 9.8 doesn't emit a `Project Unit Id` field in
          # `ghc --info` and registers its boot packages without
          # the `-inplace` suffix (`base-4.18.3.0` rather than
          # `base-4.18.3.0-inplace`).  Skip the field and the
          # suffix below for those versions so cabal computes
          # UnitIds against the dummy that match what it would
          # compute against the real GHC.
          if pkgs.lib.versionAtLeast ghc.version "9.8"
            then ''echo ',("Project Unit Id","ghc-${ghc.version}-inplace")' ''
            else ""
        }
        ${
          # Capability fields cabal-install reads to decide what
          # configure-args to record in plan.json's per-pkg
          # `configure-args` entries.  Without these, cabal assumes
          # the compiler can't build shared libs / dynamic-too,
          # so it records `--disable-shared` /
          # `--disable-library-for-ghci` — which feeds into the
          # package's UnitId hash.  Mirror real GHC's capabilities
          # so plan-to-nix's recorded ids match what cabal would
          # compute against the actual compiler.
          #
          # The values are conditioned on the *target* platform
          # because cabal reads `ghc --info` to decide things like
          # `--enable-shared` vs `--disable-shared`, and those
          # decisions feed into the package's UnitId hash.
          #
          # Reference outputs (verified by running `ghc --info` on
          # the actual cross GHC derivation in /nix/store):
          #
          #   * x86_64-w64-mingw32 (Windows mingw): no
          #     `Support shared libraries` field at all,
          #     `Support dynamic-too: NO`, `GHC Dynamic: NO`,
          #     RTS ways without any `_dyn` ways, Stage 1.
          #   * ghcjs / wasm: built stage-1 with only `v debug` ways
          #     and no dynamic linking.
          #   * native Linux/Darwin: dynamic everything, Stage 2.
          if pkgs.stdenv.targetPlatform.isGhcjs
             || pkgs.stdenv.targetPlatform.isWasm
          then
            # wasm (wasi32 / wasm32) gained an interpreter and
            # dynamic-RTS-ways support in GHC 9.12+.  Real GHC for
            # 9.12+ wasm reports:
            #   `Tables next to code: NO`
            #   `Have interpreter: YES`, `Use interpreter: YES`
            #   `RTS ways: v debug debug_dyn dyn`
            # while earlier wasm builds and all ghcjs builds keep
            # the legacy stage-1-without-interpreter values.
            let newWasm = pkgs.stdenv.targetPlatform.isWasm
                       && pkgs.lib.versionAtLeast ghc.version "9.12";
            in ''
            echo ',("Support dynamic-too","YES")'
            echo ',("Support reexported-modules","YES")'
            echo ',("Support thinning and renaming package flags","YES")'
            echo ',("Tables next to code","${if newWasm then "NO" else "YES"}")'
            echo ',("Have interpreter","${if newWasm then "YES" else "NO"}")'
            echo ',("Use interpreter","${if newWasm then "YES" else "NO"}")'
            echo ',("Have native code generator","YES")'
            echo ',("target RTS linker only supports shared libraries","NO")'
            echo ',("GHC Dynamic","NO")'
            echo ',("RTS ways","${if newWasm then "v debug debug_dyn dyn" else "v debug"}")'
            echo ',("Stage","1")'
          ''
          else if pkgs.stdenv.targetPlatform.isWindows
          then ''
            echo ',("Support dynamic-too","NO")'
            echo ',("Support reexported-modules","YES")'
            echo ',("Support thinning and renaming package flags","YES")'
            echo ',("Tables next to code","YES")'
            echo ',("Have interpreter","YES")'
            echo ',("Use interpreter","YES")'
            echo ',("Have native code generator","YES")'
            echo ',("target RTS linker only supports shared libraries","NO")'
            echo ',("GHC Dynamic","NO")'
            echo ',("RTS ways","v thr thr_debug thr_debug_p thr_p debug debug_p p")'
            echo ',("Stage","1")'
          ''
          else if pkgs.stdenv.targetPlatform.isAndroid
               || pkgs.stdenv.targetPlatform.isStatic
               || (pkgs.stdenv.buildPlatform.parsed.cpu.name != pkgs.stdenv.targetPlatform.parsed.cpu.name
                   && pkgs.stdenv.targetPlatform.isLinux)
          then ''
            # Cross-built linux GHC (android / aarch64-multiplatform
            # / etc.) or pkgsStatic: GHC built without dynamic
            # support — RTS ways have no `_dyn` family,
            # `GHC Dynamic: NO`, and no `Support shared libraries`
            # field at all (cabal interprets absence as no-shared).
            # Falling through to the "otherwise" branch makes
            # plan-to-nix record `--enable-shared`, which the
            # slice's real ghc silently flips to `--disable-shared`,
            # forking the UnitId on `pkgHashSharedLib`.
            #
            # Stage = 1 whenever the cpu architecture differs
            # between build and target (real cross-compiler, only
            # bootstrapped one stage); Stage = 2 for same-arch
            # variants like pkgsStatic on x86_64-musl from
            # x86_64-gnu (the GHC is fully bootstrapped because
            # the cpu matches the build host).
            #
            # Reference: `aarch64-unknown-linux-android-ghc --info`,
            # `aarch64-unknown-linux-gnu-ghc --info`, and the
            # pkgsStatic `x86_64-unknown-linux-musl-ghc --info`.
            echo ',("Support dynamic-too","YES")'
            echo ',("Support reexported-modules","YES")'
            echo ',("Support thinning and renaming package flags","YES")'
            echo ',("Tables next to code","YES")'
            echo ',("Have interpreter","YES")'
            echo ',("Use interpreter","YES")'
            echo ',("Have native code generator","YES")'
            echo ',("target RTS linker only supports shared libraries","NO")'
            echo ',("GHC Dynamic","NO")'
            echo ',("RTS ways","v thr thr_debug thr_debug_p thr_p debug debug_p p")'
            echo ',("Stage","${if pkgs.stdenv.buildPlatform.parsed.cpu.name != pkgs.stdenv.targetPlatform.parsed.cpu.name then "1" else "2"}")'
          ''
          else ''
            # Native (Linux / Darwin / etc.).  Real GHC 9.14.1 omits
            # `Support shared libraries` on these — cabal infers it
            # from `Support dynamic-too` and `RTS ways` instead — so
            # we omit it here too, keeping the dummy and real `--info`
            # outputs byte-equivalent (verified by
            # `tests.dummy-ghc-info`).
            echo ',("Support dynamic-too","YES")'
            echo ',("Support reexported-modules","YES")'
            echo ',("Support thinning and renaming package flags","YES")'
            echo ',("Tables next to code","YES")'
            echo ',("Have interpreter","YES")'
            echo ',("Use interpreter","YES")'
            echo ',("Have native code generator","YES")'
            echo ',("target RTS linker only supports shared libraries","NO")'
            echo ',("GHC Dynamic","YES")'
            # Real `ghc --info` RTS-ways strings (verified per-version
            # against the actual cross / native GHCs):
            #
            #   * 9.6 / 9.8 (nixpkgs bootstrap native, non-musl):
            #     10-way, weird ordering, no `v`/`p` and no
            #     `_p_dyn` variants.
            #   * 9.10 native, OR any pre-9.12 build that targets
            #     musl (haskell.nix's musl64 ghc): 12-way, includes
            #     `v` and `p` but no `_p_dyn` variants.
            #   * 9.12 / 9.14: full 16-way including `_p_dyn`.
            #
            # The string is compared byte-for-byte, so the order
            # must match verbatim.  Cross GHCs with a different cpu
            # take a different branch above (8-way set without any
            # `_dyn`); this rule is only for native-cpu targets.
            ${if pkgs.lib.versionAtLeast ghc.version "9.12"
              then ''echo ',("RTS ways","v thr thr_debug thr_debug_p thr_debug_p_dyn thr_debug_dyn thr_p thr_p_dyn thr_dyn debug debug_p debug_p_dyn debug_dyn p p_dyn dyn")' ''
              else if pkgs.lib.versionAtLeast ghc.version "9.10"
                   || pkgs.stdenv.targetPlatform.isMusl
              then ''echo ',("RTS ways","v thr thr_debug thr_debug_p thr_debug_dyn thr_p thr_dyn debug debug_p debug_dyn p dyn")' ''
              else ''echo ',("RTS ways","debug thr thr_debug thr_p dyn debug_dyn thr_dyn thr_debug_dyn thr_debug_p debug_p")' ''
            }
            echo ',("Stage","2")'
          ''
        }
        echo ']'
        ;;
      --print-libdir*)
        echo $out/ghc/libdir
        ;;
      *)
        echo "Unknown argument '$*'" >&2
        exit 1
        ;;
      esac
    exit 0
  '';
}
