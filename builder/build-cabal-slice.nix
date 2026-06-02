# Proof-of-concept builder for a cabal-store slice.
#
# Runs `cabal v2-build` in an isolated, pre-populated store and captures
# the store entries that this invocation produced.  Compose multiple
# slices by passing them as `depSlices` to a downstream call.
#
# Caller writes its own cabal.project and stages its own sources in the
# `preBuild` hook, which runs inside the build dir.  By the end of
# `preBuild`, the working directory must be the project root (a directory
# containing `cabal.project`).
#
# Output layout:
#   $out/store/ghc-<ver>[-inplace]/<unit-id>/        -- newly installed unit
#   $out/store/ghc-<ver>[-inplace]/package.db/<unit-id>.conf
#   $out/plan.json                                   -- recorded slice plan (lifted from cache)
#
# Note: `dist-newstyle/` is staged into `$out` during the build
# so `comp-v2-builder.nix`'s test/bench install step can grab the
# unpinged binary, but is trimmed to just `plan.json` at end of
# installPhase (see `trimDistNewstyle`) — downstream consumers
# never need the source-tarball / build-tree bulk.
#
# Composition: each dep's $out/store/... is copied into the downstream
# slice's starting store.  Only units NOT already present in that
# starting store are emitted to the downstream's $out/store/, which
# keeps each slice's hash sensitive only to its own inputs.
#
# POC limitations:
#   - No cross-compilation.
#   - No haddock, no test suites.
#   - No per-component selection within a local package beyond whatever
#     `target` resolves to.
{ stdenv, lib, ghc, cabal-install, pkgs, pkgsBuildBuild, buildPackages, haskellLib, makeGhcShim }@outerArgs:

let outerGhc = ghc; in

{ pname                       # `<pkg>-<ctype>-<cname>` (or
                              # with a `-doc` suffix for haddock
                              # slices).  Combined with `version`
                              # to give the derivation name; on
                              # cross, stdenv inserts the host
                              # platform's config between them so
                              # the resulting drv name matches v1's
                              # `<pkg>-<ctype>-<cname>-<crossSuffix>-<version>`
                              # convention.
, version                     # The package's `pkgVersion`, used
                              # directly as the derivation `version`
                              # attribute.
, hardeningDisable ? []       # Per-slice stdenv `hardeningDisable`
                              # list — strips the named hardening
                              # flags from `NIX_HARDENING_ENABLE`.
                              # Forwarded from the slice's
                              # `component.hardeningDisable` module
                              # option by `comp-v2-builder.nix`.
, depSlices ? []
, localRepo ? null           # derivation with <pkg>-<ver>.tar.gz files
, v2Fragment ? null          # per-package build-time-composition fragment
                             # (see comp-v2-builder.nix `v2Fragment`).  When
                             # set, the installPhase writes this package's own
                             # repo entry + cabal.project blocks + constraint +
                             # sublib seeds + lib-dep pointer into `$out`, so a
                             # downstream slice can compose the closure at build
                             # time without walking deps in Nix.  STEP 1:
                             # emitted but not yet consumed.
, preBuild                   # stage sources, write cabal.project, cd into project dir
, target ? "all"
, extraSublibSeeds ? []      # list of `{ pkg = ...; sublib = ...; }`
                             # records that the patched
                             # `prune-unreachable-sublibs.patch` should
                             # add as extra reachability seeds in
                             # cabal's solver index — required when
                             # the slice's `target` is itself a sublib
                             # of `pkg` (otherwise the unreachable-from-
                             # main-lib pruning drops it and cabal
                             # raises [Cabal-7127] "package does not
                             # contain a library with that name").
                             # Set automatically by `comp-v2-builder.nix`
                             # for sublib slices.  Only the targeted
                             # sublib (and its transitively-required
                             # sublibs) are kept — unrelated sublibs
                             # like `lib:testlib` are still pruned, so
                             # their deps (e.g. QuickCheck) don't
                             # leak into the install plan.
, passthru ? {}              # extra attrs to attach to the derivation
, extraBuildInputs ? []      # e.g. component.libs / frameworks — things ld needs at link time
, extraNativeBuildInputs ? [] # e.g. build-tools — tools on PATH at build time
, propagated ? []            # things to put in `propagatedBuildInputs`
                             # so downstream slices (using this slice as a
                             # buildInput) inherit them automatically via
                             # stdenv's propagation chain.
, ghc ? outerGhc             # caller may override (e.g. swap in a wrapped
                             # ghc for cross-TH); defaults to the outer `ghc`.
, buildToolBinOverlays ? []  # list of `{ name; buildSlice; }` entries.
                             # After composing the starting cabal-store,
                             # replace `<storeDir>/<ghc>/<exe-unit>/bin/<name>`
                             # with the build-platform binary from
                             # `${buildSlice}/bin/${name}`.  cabal v2-build
                             # prepends each build-tool unit's `bin/` to
                             # PATH before invoking ghc — if the composed
                             # unit is the cross-target one (which it is
                             # on cross, so cabal recognises its unit-id
                             # from plan-nix), ghc's `-pgmF` preprocessor
                             # lookup picks the host-arch binary and fails
                             # at `posix_spawnp` with "Exec format error".
                             # Overlaying the build-platform binary keeps
                             # the cross-target unit-id (so cabal's solver
                             # is happy) but lets the executable actually
                             # run on the build machine.
, expectedPackage ? null     # if non-null, the only package name
                             # cabal's dry-run plan is allowed to
                             # build.  Any other package means cabal
                             # couldn't reuse a unit we composed into
                             # the starting store (typically a
                             # UnitId-hash input mismatch).
, expectedUnitId ? null      # if non-null, the unit-id cabal must
                             # actually produce inside this slice's
                             # `$out/store/...`.  Sourced from the
                             # project's plan-nix.json (the canonical
                             # plan).  After the build's installPhase
                             # captures unit-ids into
                             # `$buildRoot/captured-unit-ids`, the
                             # captured set must equal `[expectedUnitId]`
                             # exactly: anything missing or extra is a
                             # fail.  Sibling unit-ids of the same
                             # package (e.g. cabal accidentally
                             # rebuilding the lib alongside an
                             # exe-only slice) count as extras here —
                             # the upstream lib slice is what
                             # downstream consumers compose, so a
                             # second copy with a different unit-id
                             # would silently fork dep hashes.
, expectedPlanEntries ? null # if non-null, an attr set { <unit-id> =
                             # <plan-json entry>; } restricted to the
                             # entries this slice expects to produce
                             # (typically just one).  When the unit-id
                             # check fails, the diagnostic diffs the
                             # corresponding plan-nix entry against
                             # the entry cabal actually produced
                             # (looked up by unit-id in the slice's
                             # `dist-newstyle/cache/plan.json`).  For
                             # deeper diagnostics across all packages
                             # see the slice's `.checkAgainstPlan`.
, dryRunOnly ? false         # if true, run only `cabal v2-build
                             # --dry-run` (no actual build), skip the
                             # unit-id check, and always succeed.
                             # Used by the `.checkAgainstPlan`
                             # diagnostic drv attached to each slice.
, planNixJsonFile ? null     # /nix/store path of the project's
                             # plan-nix.json (full `install-plan`
                             # list).  When set together with
                             # `dryRunOnly`, the install phase
                             # iterates every entry in the slice's
                             # `dist-newstyle/cache/plan.json`,
                             # finds the matching entry in plan-nix
                             # (by id, otherwise by pkg-name +
                             # component) and emits a `jq -S | diff`
                             # of the two with full file context.
                             # NOT a build-time dep of the slice
                             # itself — only the sibling
                             # `checkAgainstPlan` drv passes it.
, doHaddock ? false          # if true, also run `cabal v2-haddock`
                             # after the build and copy the resulting
                             # html tree to `$out/share/doc/<pkg>/html`.
                             # Used by `comp-v2-builder.nix`'s
                             # sibling `docSlice` derivation to
                             # generate haddock without forking the
                             # main library slice's UnitId.  The
                             # main `cabal v2-build` is still run so
                             # the slice's `$out/store/...` is
                             # populated and downstream consumers
                             # (testers / IDE tools) can use the doc
                             # slice as a drop-in for the lib slice
                             # when they want both bytes and docs.
, withProgFlags ? ""         # extra `--with-PROG=PATH` flags
                             # appended to the `cabal v2-build`
                             # command (wrapped in
                             # `--configure-option=` so cabal threads
                             # them through to per-package Setup
                             # configure — `cabal v2-build` itself
                             # rejects arbitrary `--with-PROG`).
                             # Used by `comp-v2-builder.nix` on cross
                             # to point cabal at the build-platform
                             # exe of each transitive build-tool;
                             # makes the build-tool's cabal-built
                             # unit-id stable across slices in the
                             # same project (every slice passes the
                             # same flags so the uid hash matches).
, allowedBuildToolPackages ? []
                             # Package names whose appearance in
                             # cabal's plan / captured-unit set is
                             # accepted in addition to
                             # `expectedPackage`.  Used on cross:
                             # cabal's solver always plans every
                             # transitive `build-tool-depends:` and
                             # may also build it, so the slice
                             # legitimately captures those units
                             # alongside the target.  Any captured
                             # pkg-name outside `expectedPackage ∪
                             # allowedBuildToolPackages` is still a
                             # fail.
, confLibraryDirs ? []       # Extra paths to append to each
                             # captured unit's `.conf` library-dirs
                             # fields, AFTER cabal's `Setup register`
                             # has run.  Used to point downstream
                             # consumers' GHC runtime linker (the
                             # one that loads packages for TH eval
                             # under `-fexternal-interpreter`) at
                             # the `extra-libraries:` entries' real
                             # foreign-lib locations (openssl/libsodium/
                             # ...).  Post-register injection is the
                             # only place we can do this without
                             # forking the slice's unit-id from
                             # plan-nix — passing `--extra-lib-dirs`
                             # at configure time would land in
                             # `pkgHashExtraLibDirs` and diverge.
                             # Downstream consumers' unit-ids aren't
                             # affected either: their config hash
                             # records each dep's unit-id, not the
                             # dep's `.conf` content.
}:

let
  # Cross-aware program names. nixpkgs' cross GHCs prefix every
  # binary with `${ghc.targetPrefix}` (e.g. `javascript-unknown-ghcjs-ghc-pkg`)
  # and don't ship the unprefixed `ghc-pkg`, so plain `ghc-pkg` on
  # PATH only exists for native builds.  Use the prefixed name
  # uniformly.
  targetPrefix = ghc.targetPrefix or "";
  ghcBin       = "${targetPrefix}ghc";
  ghcPkgBin    = "${targetPrefix}ghc-pkg";

  # Extra `--with-PROG` flags to pass to cabal so it picks up the
  # cross compiler's tools instead of looking for unprefixed names.
  # GHC-related tools key off `ghc.targetPrefix`; the C toolchain
  # (gcc / ld / ar / strip) keys off `stdenv.cc.bintools.targetPrefix`,
  # which can differ from `ghc.targetPrefix` (notably for ghcjs,
  # where ghc is `javascript-unknown-ghcjs-` but the C toolchain
  # is emscripten with no prefix).
  ccPrefix = if stdenv.hasCC or (stdenv.cc != null)
             then stdenv.cc.targetPrefix else "";
  binPrefix = if stdenv.hasCC or (stdenv.cc != null)
              then stdenv.cc.bintools.targetPrefix else "";
  # On cross, `cabalPkgConfigWrapper` is named
  # `${targetPrefix}pkg-config` (e.g. `x86_64-w64-mingw32-pkg-config`).
  # Cabal calls plain `pkg-config` by default, so for cross we have
  # to point it at the prefixed binary via `--with-pkg-config=`.
  # Mirrors comp-builder.nix:345.
  pkgConfigPrefix = buildPackages.cabalPkgConfigWrapper.targetPrefix;
  # Only emit cross-aware overrides; for native, cabal finds
  # everything on PATH itself.  Avoiding `--with-PROG=` flags on
  # native is also important for UnitId stability — every entry
  # in `--with-PROG=` enters cabal's `pkgHashProgramArgs`, so
  # passing them in the slice build but not in the user's later
  # `cabal v2-build` invocation forks the UnitId and forces a
  # rebuild downstream.
  # `cabal v2-build` only accepts `--with-compiler=` — it has no
  # per-tool `--with-ghc-pkg=` flag and instead does a "near
  # compiler" lookup expecting an unprefixed `ghc-pkg` in the same
  # directory.  Cross GHCs ship only `<prefix>ghc-pkg`, so we build a
  # tiny symlink farm that exposes unprefixed names alongside the
  # prefixed ones and point `--with-compiler=` at it.
  # Two-pass: link every source file under its own name first, then
  # synthesise unprefixed aliases ONLY for prefixed names that have
  # no real unprefixed sibling in the source bin/.  GHC 9.14.1's
  # android cross ships both `<prefix>deriveConstants` and a real
  # `deriveConstants` (for build-host use); the previous single-pass
  # loop hit the prefixed name first (alphabetical glob), created an
  # alias `$out/bin/deriveConstants -> <prefix>deriveConstants`, then
  # crashed on the real `deriveConstants`'s raw `ln -s` with "File
  # exists".
  # When cabal v2-build instantiates a backpack signature on ghcjs,
  # GHC's JS backend may not emit a `.o` for the signature module
  # (the signature is filled by an external implementation unit).
  # Cabal then asks `emar` to archive both `Module.o` and `Consumer.o`,
  # and `llvm-ar` aborts because `Module.o` is absent.  This wrapper
  # filters out missing `.o` entries (both inline argv and in
  # response files) before delegating to the real `emar`.  Pass-through
  # when nothing is missing.
  #
  # Cabal's `--with-ar=` flag would have been the natural lever, but
  # cabal v2-build's internal "Building library" step reads
  # `ar command` from GHC's `settings` file rather than honouring
  # `--with-ar=`.  So instead we materialise a writable settings file
  # in the shim and `sed`-swap the `ar command` entry (matching the
  # approach v1 takes in `comp-builder.nix`).
  ghcjsArWrapper = pkgsBuildBuild.writeShellScript "ghcjs-ar-wrapper" ''
    REAL_AR=${pkgsBuildBuild.emscripten}/bin/emar
    args=()
    for arg in "$@"; do
      if [[ "$arg" == @* ]]; then
        rspfile="''${arg#@}"
        newrsp="''${rspfile}.filtered"
        while IFS= read -r line || [ -n "$line" ]; do
          if [[ "$line" == *.o ]] && [[ ! -e "$line" ]]; then
            continue
          fi
          echo "$line"
        done < "$rspfile" > "$newrsp"
        args+=("@$newrsp")
      elif [[ "$arg" == *.o ]] && [[ ! -e "$arg" ]]; then
        continue
      else
        args+=("$arg")
      fi
    done
    exec "$REAL_AR" "''${args[@]}"
  '';

  ghcShim = makeGhcShim {
    inherit ghc ghcjsArWrapper;
    # iserv-dyn dlopens our haskell `.so` deps at TH eval time; on
    # native-musl those transitively pull in `libstdc++.so` (e.g. via
    # `double-conversion`).  Musl's `libstdc++.so` has DT_RUNPATH
    # pointing only at musl's libc dir, not at the gcc lib dir where
    # `libgcc_s.so.1` lives — and musl's dyld only consults the
    # immediate library's RUNPATH, so the lookup fails.  Have the
    # ghc shim prefix `LD_LIBRARY_PATH` with the musl-gcc libs dir
    # for ghc and its children (iserv-dyn) only.  v1 sets the env
    # globally (`comp-builder.nix:507`); doing it derivation-wide
    # breaks glibc `git` for `source-repository-package` resolution.
    extraLibraryPaths = lib.optional
      haskellLib.isNativeMusl
      "${buildPackages.gcc-unwrapped.lib}/${stdenv.hostPlatform.config}/lib";
  };

  crossWithFlags = lib.optionalString (targetPrefix != "") (
    " --with-compiler=${ghcShim}/bin/${ghcBin}"
    + " --with-pkg-config=${pkgConfigPrefix}pkg-config"
    + lib.optionalString (stdenv.hasCC or (stdenv.cc != null)) (
        # CC
        (if stdenv.hostPlatform.isGhcjs
         then " --with-gcc=${pkgsBuildBuild.emscripten}/bin/emcc"
         else " --with-gcc=${ccPrefix}cc")
        # BINTOOLS
      + " --with-ar=${binPrefix}ar"
      + " --with-strip=${binPrefix}strip"
        # Starting with ghc 9.10 the `ld command` is no longer in
        # the GHC settings file; pass it explicitly.  ghcjs uses
        # emcc as the linker.
      + (if stdenv.hostPlatform.isGhcjs
         then " --with-ld=${pkgsBuildBuild.emscripten}/bin/emcc"
         else lib.optionalString
                (builtins.compareVersions ghc.version "9.8" >= 0)
                " --with-ld=${binPrefix}ld")
    )
  );
in

stdenv.mkDerivation ({
  inherit pname version hardeningDisable;
  # GHC / hsc2hs / cabal write/read source files; without a UTF-8
  # locale they fall back to the C encoding and crash on non-ASCII
  # input (e.g. `commitBuffer: invalid argument (cannot encode
  # character '\194')` when hsc2hs hits a degree symbol).  Mirrors
  # `comp-builder.nix:459-461,503`.
  LANG = "en_US.UTF-8";
  LC_ALL = "en_US.UTF-8";
} // lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc") {
  LOCALE_ARCHIVE = "${buildPackages.glibcLocales}/lib/locale/locale-archive";
} // lib.optionalAttrs (extraSublibSeeds != []) {
  # Read by the patched `prune-unreachable-sublibs.patch` to add
  # extra reachability seeds when walking each package's sublib graph.
  # Format: comma-separated `pkg/sublib` entries.  Set by
  # `comp-v2-builder.nix` for slices whose `target` is itself a sublib.
  HASKELLNIX_EXTRA_SUBLIB_SEEDS =
    lib.concatMapStringsSep "," (s: "${s.pkg}/${s.sublib}") extraSublibSeeds;
} // {
  # GHCJS runs Template Haskell splices by linking the splice as a
  # JS module and executing it via node at compile-time, so node has
  # to be on PATH inside this build env (just like haskellLib.check
  # for ghcjs binaries).  Mirrors the same `optional isGhcjs nodejs`
  # in lib/check.nix.
  #
  # `lndir` is used in the buildPhase to compose dep slices into
  # `$out/store` as a symlink tree — see comments there.
  # `cabalPkgConfigWrapper` (haskell.nix's build-time pkg-config
  # wrapper, matching comp-builder.nix:432) is needed when any
  # package in the cabal plan has `pkgconfig-depends:` — cabal's
  # solver invokes pkg-config to query module versions.  This slice
  # re-runs the solver for the subset of packages it can see, so
  # rather than plumb a per-slice flag we always include it.
  nativeBuildInputs = [ ghc cabal-install
                        (pkgsBuildBuild.lndir or pkgsBuildBuild.xorg.lndir)
                        buildPackages.cabalPkgConfigWrapper
                        # `jq` is used by the unit-id mismatch diagnostic
                        # to extract / pretty-print plan.json entries for
                        # the diff between plan-nix and the slice's
                        # actual `dist-newstyle/cache/plan.json`.
                        pkgsBuildBuild.jq
                        # `gawk` for the per-slice diagnostics (extras
                        # filter, conf parsing) and for darwin stdenv's
                        # own setup.sh which calls `awk` during fixup —
                        # missing on darwin's default sandbox PATH so
                        # we plumb it through nativeBuildInputs.
                        pkgsBuildBuild.gawk
                        # cabal-install clones `source-repository-package`
                        # blocks via `git clone file://...`, which needs
                        # `git` on PATH (cabal raises [Cabal-6666] "The
                        # program 'git' is required but it could not be
                        # found." otherwise).
                        #
                        # On native-musl pick the host (musl) git over
                        # the build (glibc) one so it isn't tripped up
                        # by `LD_LIBRARY_PATH=<musl-gcc-libs>` (set by
                        # the ghc shim for iserv's libgcc lookup at
                        # TH-eval time) when ghc/iserv invoke `git`
                        # from a TH callback.  `test/githash` does the
                        # same.  `pkgs.gitReallyMinimal` would be
                        # auto-spliced back to the build platform by
                        # `mkDerivation`'s `nativeBuildInputs`
                        # handling, so reach through `pkgsHostHost`,
                        # which exposes a leaf with no `__spliced`.
                        (if haskellLib.isNativeMusl
                         then pkgs.pkgsHostHost.gitReallyMinimal
                         else pkgsBuildBuild.gitReallyMinimal) ]
    ++ lib.optional stdenv.hostPlatform.isGhcjs pkgsBuildBuild.nodejs
    ++ extraNativeBuildInputs;
  # `depSlices` go in `propagatedBuildInputs` so stdenv chains each
  # slice's `nix-support/propagated-build-inputs` transitively.
  # Concretely: `cardano-lmdb:lib:ffi` declares `pkgconfig = [lmdb]`
  # in its own `propagated`; with chaining, `cardano-lmdb:lib:cardano-lmdb`
  # records lmdb in its own propagation file, and a downstream
  # consumer (`cardano-lmdb-simple`) gets lmdb in its build env
  # automatically — no manual `transitiveDepLibs` walk required.
  # Normal nixpkgs deps like `lmdb` carry `__spliced` so they
  # auto-swap to the consumer's pkg-set under cross-compilation; the
  # only entries that don't auto-swap are the slice $outs themselves
  # (ad-hoc mkDerivation results), which are mostly inert directories
  # of cabal-store symlinks anyway.
  #
  # The buildPhase finds slices by walking `pkgsHostTarget` (the bash
  # array stdenv builds from buildInputs *and* propagatedBuildInputs)
  # for entries that expose a `store/` subdir.  Same pattern as v1's
  # `make-config-files.nix:102-114`.  Slices have no `lib/`/`include/`
  # at $out so stdenv's NIX_LDFLAGS / NIX_CFLAGS injection finds
  # nothing to add — propagation is for iteration + transitive
  # sysLib chaining only.
  buildInputs = extraBuildInputs;
  propagatedBuildInputs = propagated ++ depSlices;
  # `fixupPhase` is what stdenv uses to write
  # `nix-support/propagated-build-inputs` from `propagatedBuildInputs`.
  # Without it the slice's `propagated` (pkgconfig deps, frameworks,
  # ...) silently fail to flow into downstream consumers — notably the
  # v2 shell, where it makes `pkg-config --list-all` miss zlib etc.
  phases = [ "buildPhase" "installPhase" "fixupPhase" ];
  # Skip stripping: strip.sh from cctools' wrapper trips on
  # `exit_code: unbound variable` under stdenv's `set -u`, and our
  # slice $out is a tree of /nix/store symlinks anyway — nothing to
  # strip that the dep slices haven't already stripped themselves.
  dontStrip = true;
  inherit passthru;

  buildPhase = ''
    set -euo pipefail

    buildRoot=$PWD

    # --- Compose starting store --------------------------------------
    # Use $out/store as the cabal store dir from the start so that
    # any path baked into a built binary (e.g. alex's `Paths_alex`
    # data-dir, which records `<storeDir>/<unit>/share/...` at
    # compile time) points at a permanent /nix/store location.  If
    # we used a $buildRoot/... path here, the binary's baked path
    # would point at an ephemeral build dir that vanishes after
    # the slice finishes — and a downstream consumer running the
    # binary (e.g. language-c invoking alex during preprocessing)
    # would fail with "AlexTemplate.hs: openFile: does not exist".
    #
    # Compose dep slices via `lndir` instead of `cp -r`: every dep
    # slice file becomes a symlink into the dep slice's $out under
    # /nix/store.  Cabal reads through the symlinks transparently;
    # nothing here actually owns those files — so when we install
    # this slice, leaving the symlinks in $out/store is fine.
    # Downstream slices composing this $out follow the symlinks back
    # to the original dep slices (kept alive by nix's GC roots).
    #
    # Find the dep slices by iterating `pkgsHostTarget` (the bash
    # array stdenv populates from `buildInputs`) and looking for
    # entries that expose a `store/` subdir — the marker for a
    # cabal slice's $out.  Same pattern as v1's
    # `make-config-files.nix:102-114` (which iterates the same
    # array looking for `package.conf.d`).
    #
    # Only *direct* slice deps are in `buildInputs` (so the closure
    # cost stays out of the nix-side eval).  Each slice writes the
    # transitive set of slices it composed in to
    # `$out/nix-support/transitive-deps` (one store path per line);
    # follow that file here to pull in the indirect ones too.
    storeDir=$out/store
    mkdir -p $storeDir
    declare -A seenDeps
    addDep() {
      local d=$1
      if [ -n "$d" ] && [ -d "$d/store" ] && [ -z "''${seenDeps[$d]:-}" ]; then
        seenDeps[$d]=1
        lndir -silent "$d/store" "$storeDir"
      fi
    }
    for dep in "''${pkgsHostTarget[@]}"; do
      addDep "$dep"
      if [ -f "$dep/nix-support/transitive-deps" ]; then
        while IFS= read -r tdep; do
          addDep "$tdep"
        done < "$dep/nix-support/transitive-deps"
      fi
    done
    # `package.db` needs to be a writable directory (cabal/ghc-pkg
    # rewrite `package.cache`, recache, etc.).  After lndir it's a
    # dir of symlinks under a real dir — fine for adding files —
    # but `package.cache` and `package.cache.lock` must be
    # writable real files, not symlinks pointing at read-only
    # /nix/store paths (otherwise `ghc-pkg recache`'s `flock()`
    # fails with `hLock: invalid argument`).  Remove the symlinks;
    # ghc-pkg will create real files in their place.
    ghcDirInitial=$(ls -d $storeDir/ghc-* 2>/dev/null | head -n1 || true)
    if [ -n "$ghcDirInitial" ]; then
      for f in package.cache package.cache.lock; do
        if [ -L "$ghcDirInitial/package.db/$f" ]; then
          rm "$ghcDirInitial/package.db/$f"
        fi
      done
    fi

    ${lib.optionalString (buildToolBinOverlays != []) ''
      # Overlay each cross-target build-tool's `bin/<name>` in the
      # composed store with the build-platform binary.  See the
      # `buildToolBinOverlays` arg's doc-string.
      ${lib.concatMapStrings (e: ''
        shopt -s nullglob
        for binFile in $storeDir/ghc-*/*-e-${e.name}-*/bin/${e.name}; do
          chmod u+w "$(dirname "$binFile")" 2>/dev/null || true
          rm -f "$binFile"
          cp -L ${e.buildSlice}/bin/${e.name} "$binFile"
          chmod u+rx "$binFile" 2>/dev/null || true
        done
        shopt -u nullglob
      '') buildToolBinOverlays}
    ''
    }# Snapshot existing unit filenames so we can identify new ones later.
    ghcDir=$(ls -d $storeDir/ghc-* 2>/dev/null | head -n1 || true)
    if [ -n "$ghcDir" ] && [ -d "$ghcDir/package.db" ]; then
      # No chmod needed — `package.db` is a real dir from lndir,
      # and the `.conf` entries in it are symlinks pointing at
      # read-only /nix/store paths.  ghc-pkg recache only needs to
      # write `package.cache`, which doesn't exist yet (we removed
      # any symlinked one above).
      ${ghcPkgBin} --package-db=$ghcDir/package.db recache
      ( cd $ghcDir/package.db && ls *.conf 2>/dev/null ) > $buildRoot/confs-before || true

      # Also snapshot the flat <ghcDir>/lib/ that cabal uses for
      # dylibs.  See installPhase for how this is used to emit only
      # *new* lib files to $out.
      if [ -d "$ghcDir/lib" ]; then
        ( cd $ghcDir/lib && ls 2>/dev/null ) > $buildRoot/lib-before || true
      else
        : > $buildRoot/lib-before
      fi

      # Snapshot all unit directories (not just those with .conf).
      # Exe-only units have a dir like <ghcDir>/<unit-id>/bin/<name>
      # but no corresponding .conf in package.db.  We need to catch
      # those too so a downstream compose can find them.
      #
      # `find -maxdepth 1 -type d` rather than `ls -d */` because the
      # OS-prefix patch on cabal-install puts `-` at the start of
      # some unit-ids (e.g. `-clsss-...`), and bash expands `*/` into
      # those names as args to `ls`, which then interprets the
      # leading `-` as flags and silently produces no output.
      ( cd $ghcDir \
          && find . -mindepth 1 -maxdepth 1 -type d \
          | sed 's|^\./||' ) \
        > $buildRoot/unitdirs-before || true
    else
      : > $buildRoot/confs-before
      : > $buildRoot/lib-before
      : > $buildRoot/unitdirs-before
    fi

    # --- CABAL_DIR and optional local-repo ---------------------------
    export HOME=$buildRoot/home
    mkdir -p $HOME/.cabal
    export CABAL_DIR=$HOME/.cabal

    # emcc is very slow if it cannot cache stuff in $HOME, and
    # newer nixpkgs default its cache dir to a /nix/store path
    # that isn't writable.  Set up a writable EM_CACHE explicitly.
    ${lib.optionalString stdenv.hostPlatform.isGhcjs ''
      export EM_CACHE=$(mktemp -d)
      if [ -d ${pkgsBuildBuild.emscripten}/share/emscripten/cache ]; then
        cp -r ${pkgsBuildBuild.emscripten}/share/emscripten/cache/* $EM_CACHE/
        chmod -R u+w $EM_CACHE
      fi
    ''}

    # Cross-TH on windows uses a wrapped ghc that prepends
    # `-fexternal-interpreter -pgmi <wineIservWrapper>`.  The
    # wrapper script reads `pkgsHostTargetAsString` (under
    # `set -u`) to find host-platform DLLs.  stdenv exposes
    # `pkgsHostTarget` as a bash array; flatten it always (use
    # `[*]` with the empty default so unset/empty arrays produce
    # an empty string rather than an `unbound variable` error).
    export pkgsHostTargetAsString="''${pkgsHostTarget[*]:-}"

    ${if v2Fragment != null then ''
      # --- Build-time slicing repo (STEP 2: composed from fragments) ---
      # Assemble the per-slice hackage repo here instead of in Nix: this
      # package's own source (passed directly — our own `repo-frag` isn't
      # built yet) plus every dep's `repo-frag/` reached through the same
      # `transitive-deps` closure walk used for the store above.  cabal
      # hashes package *source* + `.cabal` descriptions (not the index
      # bytes or repo path), so a content-equivalent repo keeps unit-ids
      # identical to the old Nix-assembled `slicingRepo`.
      v2repo=$buildRoot/local-repo
      v2idx=$(mktemp -d)
      mkdir -p $v2repo/package
      declare -A seenRepoFrag
      addRepoFrag() {
        local f=$1
        if [ -n "$f" ] && [ -d "$f/repo-frag" ] && [ -z "''${seenRepoFrag[$f]:-}" ]; then
          seenRepoFrag[$f]=1
          for t in "$f"/repo-frag/package/*; do
            [ -e "$t" ] && ln -sf "$t" "$v2repo/package/$(basename "$t")"
          done
          if [ -d "$f/repo-frag/index" ]; then
            # `-n` (no-clobber): a multi-sublib package (e.g. happy-lib's
            # grammar/frontend/tabular) appears under several fragment
            # paths that all carry the *same* `<pkg>/<ver>/<pkg>.cabal`;
            # the .cabal is identical, so skip duplicates rather than
            # re-copy onto the read-only file the first copy left.
            cp -rL --no-preserve=mode -n "$f"/repo-frag/index/. "$v2idx"/
          fi
        fi
      }
      # This package's own source + .cabal (own fragment not built yet).
      ln -s ${v2Fragment.tarball} $v2repo/package/${v2Fragment.pkgName}-${v2Fragment.pkgVersion}.tar.gz
      mkdir -p $v2idx/${v2Fragment.pkgName}/${v2Fragment.pkgVersion}
      ${if v2Fragment.cabalFile != null
        then "cp ${v2Fragment.cabalFile} $v2idx/${v2Fragment.pkgName}/${v2Fragment.pkgVersion}/${v2Fragment.pkgName}.cabal"
        else ''
          tar -xOzf ${v2Fragment.tarball} \
            ${v2Fragment.pkgName}-${v2Fragment.pkgVersion}/${v2Fragment.pkgName}.cabal \
            > $v2idx/${v2Fragment.pkgName}/${v2Fragment.pkgVersion}/${v2Fragment.pkgName}.cabal
        ''}
      # Dep closure repo-frags (direct + transitive — same walk as store).
      for dep in "''${pkgsHostTarget[@]}"; do
        addRepoFrag "$dep"
        if [ -f "$dep/nix-support/transitive-deps" ]; then
          while IFS= read -r tdep; do addRepoFrag "$tdep"; done < "$dep/nix-support/transitive-deps"
        fi
      done
      # Hackage index tarball (sorted, fixed mtime — as the Nix repo did).
      tar --sort=name --mtime='@0' --owner=0 --group=0 --numeric-owner \
          -czf $v2repo/00-index.tar.gz -C $v2idx .
      cat > $CABAL_DIR/config <<EOF
      repository hackage.haskell-nix
        url: file://$v2repo
        secure: False
      EOF
      cabal update hackage.haskell-nix
    '' else lib.optionalString (localRepo != null) ''
      # Hackage-style local repo: `00-index.tar.gz` + `package/<pkg>-<ver>.tar.gz`.
      # See comp-v2-builder.nix `slicingRepo` for why we use this layout
      # over `file+noindex://`.  `secure: False` skips TUF root-key checks
      # (we don't sign this repo); the cabal.project then sets
      # `active-repositories: hackage.haskell-nix` so the solver only sees
      # this repo (no other repository is declared in CABAL_DIR/config).
      # cabal only reads from the repo dir at solve time (its mutable
      # index cache lives under `$CABAL_DIR/packages/`), so we point
      # cabal at the `localRepo` /nix/store path directly — no copy
      # into $buildRoot, no chmod -- just reference it in place.
      cat > $CABAL_DIR/config <<EOF
      repository hackage.haskell-nix
        url: file://${localRepo}
        secure: False
      EOF

      # `file://` repos need `cabal update` to populate
      # `$CABAL_DIR/packages/<repo>/` from the repo's
      # `00-index.tar.gz` before the solver can see anything.  This
      # is purely local IO — no network.
      cabal update hackage.haskell-nix
    ''}

    # --- Caller hook: stage sources + write cabal.project ------------
    mkdir -p $buildRoot/project
    cd $buildRoot/project
    ${preBuild}

    ${lib.optionalString (v2Fragment != null) ''
      # --- Compose cabal.project from per-package fragments ----------
      # The cabal.project is built entirely here, at build time: a local
      # skeleton (`v2Fragment.localCabalProject`) plus the closure-derived
      # sections (extra-packages / per-package block groups / allow-boot /
      # constraints) composed from the per-package fragments — so the
      # dependency-closure walk never happens in the Nix evaluator.
      # cabal hashes content not field order, so local-then-closure is
      # equivalent to the old Nix-assembled project (verified by an
      # order-insensitive diff while this was being developed).

      # Self's fragment (its own $out isn't built yet) materialised from
      # the passed values, so self participates in the block groups.
      selfFrag=$(mktemp -d); mkdir -p $selfFrag/nix-support/v2-frag
      printf '%s' ${lib.escapeShellArg v2Fragment.pkgName} > $selfFrag/nix-support/v2-frag/pkg-name
      cp ${builtins.toFile "flags"             v2Fragment.flagsBlock}            $selfFrag/nix-support/v2-frag/flags
      cp ${builtins.toFile "ghc-options"       v2Fragment.ghcOptionsBlock}       $selfFrag/nix-support/v2-frag/ghc-options
      cp ${builtins.toFile "configure-options" v2Fragment.configureOptionsBlock} $selfFrag/nix-support/v2-frag/configure-options
      cp ${builtins.toFile "program-options"   v2Fragment.programOptionsBlock}   $selfFrag/nix-support/v2-frag/program-options
      cp ${builtins.toFile "doc"               v2Fragment.docBlock}              $selfFrag/nix-support/v2-frag/doc
      cp ${builtins.toFile "extra-lib-dirs"    v2Fragment.extraLibDirsBlock}     $selfFrag/nix-support/v2-frag/extra-lib-dirs
      cp ${builtins.toFile "sublib-seeds" (lib.concatMapStrings (s: s + "\n") v2Fragment.sublibSeeds)} \
        $selfFrag/nix-support/v2-frag/sublib-seeds
      cp ${builtins.toFile "setup-constraints" (lib.concatMapStrings (s: s + "\n") v2Fragment.setupConstraints)} \
        $selfFrag/nix-support/v2-frag/setup-constraints

      # all-dep closure fragments (blocks scope): self + direct + transitive.
      declare -A seenBlk; blkFrags=()
      addBlk() { local f=$1; if [ -n "$f" ] && [ -d "$f/nix-support/v2-frag" ] && [ -z "''${seenBlk[$f]:-}" ]; then seenBlk[$f]=1; blkFrags+=("$f"); fi; }
      addBlk "$selfFrag"
      for dep in "''${pkgsHostTarget[@]}"; do
        addBlk "$dep"
        [ -f "$dep/nix-support/transitive-deps" ] && while IFS= read -r d; do addBlk "$d"; done < "$dep/nix-support/transitive-deps"
      done

      # lib-dep closure fragments (extra-packages + constraints scope).
      declare -A seenLib; libFrags=()
      addLib() { local f=$1; if [ -n "$f" ] && [ -d "$f/nix-support/v2-frag" ] && [ -z "''${seenLib[$f]:-}" ]; then seenLib[$f]=1; libFrags+=("$f"); fi; }
      ${lib.concatMapStrings (s: ''
        addLib ${s}
        [ -f ${s}/nix-support/lib-dep-slices ] && while IFS= read -r d; do addLib "$d"; done < ${s}/nix-support/lib-dep-slices
      '') v2Fragment.libDepSlices}
      # Build tools contribute their *lib* closures (not the exe pkg),
      # matching `exeUnitsInAllDeps` in `libConstraintPins`.
      ${lib.concatMapStrings (s: ''
        [ -f ${s}/nix-support/lib-dep-slices ] && while IFS= read -r d; do addLib "$d"; done < ${s}/nix-support/lib-dep-slices
      '') v2Fragment.exeDepSlices}

      # Emit "<pkg-name>\t<frag-dir>" name-sorted.
      # Emit one "<pkg-name>\t<frag-dir>" line per *pkg-name*, name-sorted.
      # `-k1,1 -u` dedups by pkg-name: a multi-sublib package contributes
      # several fragments (one per sublib slice) that all share the same
      # pkg-name and emit identical per-package content (block / constraint /
      # extra-package), so we keep just one — matching how the Nix side
      # dedups via `sliceCanonicalNames` / `libConstraintPins` (by name).
      sortedByName() { local f; for f in "$@"; do printf '%s\t%s\n' "$(cat "$f"/nix-support/v2-frag/pkg-name)" "$f"; done | sort -t"$(printf '\t')" -k1,1 -u; }

      {
        # extra-packages: self entry first, then lib-dep pins (name-sorted).
        epline=""; epsep=""
        ${lib.optionalString (v2Fragment.selfExtraPackage != "")
          ''epline=${lib.escapeShellArg v2Fragment.selfExtraPackage}; epsep=", "''}
        while IFS=$'\t' read -r name f; do
          [ -n "$name" ] || continue
          c=$(cat "$f"/nix-support/v2-frag/constraint); epline="$epline$epsep''${c%%,*}"; epsep=", "
        done < <(sortedByName "''${libFrags[@]}")
        [ -n "$epline" ] && echo "extra-packages: $epline"

        # allow-boot-library-installs when self or a lib-dep pin is a boot lib.
        bootlibs=" ${lib.concatStringsSep " " v2Fragment.bootLibPkgNames} "; needBoot=0
        case "$bootlibs" in *" ${v2Fragment.pkgName} "*) needBoot=1 ;; esac
        while IFS=$'\t' read -r name f; do case "$bootlibs" in *" $name "*) needBoot=1 ;; esac; done < <(sortedByName "''${libFrags[@]}")
        [ $needBoot -eq 1 ] && echo "allow-boot-library-installs: True"

        # Six block groups, grouped by type, name-sorted across the closure.
        for blk in flags ghc-options configure-options program-options doc extra-lib-dirs; do
          while IFS=$'\t' read -r name f; do cat "$f/nix-support/v2-frag/$blk"; done < <(sortedByName "''${blkFrags[@]}")
        done

        # constraints: self `any.<pkg> source`, then lib-dep pins (name-sorted).
        echo "constraints: any.${v2Fragment.pkgName} source"
        while IFS=$'\t' read -r name f; do
          [ -n "$name" ] || continue
          echo "constraints: $(cat "$f"/nix-support/v2-frag/constraint)"
        done < <(sortedByName "''${libFrags[@]}")

        # Per-package custom-setup pins (`<pkg>:setup.<dep> ==<ver>`),
        # collected across the all-dep closure so Custom-build packages'
        # setups (notably their `Cabal`) resolve as plan-nix recorded —
        # keeping their unit-ids reproducible (e.g. ghc-paths).
        for f in "''${blkFrags[@]}"; do
          [ -f "$f/nix-support/v2-frag/setup-constraints" ] && \
            sed 's/^/constraints: /' "$f/nix-support/v2-frag/setup-constraints" || true
        done
      } > cabal.project.closure

      # local skeleton (via file — no shell escaping) + composed sections.
      # Use a redirect (not `cp`, which would create cabal.project with
      # the store file's read-only mode and break the `>>` append).
      cat ${builtins.toFile "local-cabal-project" v2Fragment.localCabalProject} > cabal.project
      cat cabal.project.closure >> cabal.project

      # Sublib reachability seeds for the prune-unreachable-sublibs patch
      # (HASKELLNIX_EXTRA_SUBLIB_SEEDS): this slice's own target sublib
      # plus every `pkg/sublib` referenced across the all-dep closure
      # (each fragment's `sublib-seeds`).  Composed here, not in Nix.
      {
        ${lib.optionalString (v2Fragment.selfTargetSublibSeed != "")
          "echo ${lib.escapeShellArg v2Fragment.selfTargetSublibSeed}"}
        for f in "''${blkFrags[@]}"; do
          # `|| true`: a missing/empty sublib-seeds must not fail the
          # loop (and, under `pipefail`, the whole pipe + `set -e`).
          [ -f "$f/nix-support/v2-frag/sublib-seeds" ] && cat "$f/nix-support/v2-frag/sublib-seeds" || true
        done
      } | sort -u | sed '/^$/d' > sublib-seeds.txt
      export HASKELLNIX_EXTRA_SUBLIB_SEEDS="$(paste -sd, sublib-seeds.txt)"
    ''}

    # --- Build -------------------------------------------------------
    if [ ! -f cabal.project ]; then
      echo "ERROR: preBuild did not produce a cabal.project" >&2
      exit 1
    fi
    echo "--- cabal.project ---"
    cat cabal.project
    # Dump the shim's .cabal too — its `build-depends` line is the
    # transitive lib-closure version pin set comp-v2-builder
    # synthesised, and is often what shapes cabal-install's solve.
    for shim_cabal in *.tar.gz; do
      case "$shim_cabal" in hs-nix-v2-shim-*.tar.gz)
        echo "--- $shim_cabal/hs-nix-v2-shim.cabal ---"
        tar -xOzf "$shim_cabal" --wildcards '*/hs-nix-v2-shim.cabal' 2>/dev/null \
          || tar -xOzf "$shim_cabal" hs-nix-v2-shim-0/hs-nix-v2-shim.cabal 2>/dev/null \
          || echo "(could not extract shim .cabal from $shim_cabal)"
        ;;
      esac
    done

    # cabal.project's `with-compiler:` only pins ghc; everything
    # else (`ghc-pkg`, `hsc2hs`, ...) has to come via CLI flags
    # because cabal.project doesn't accept `with-PROG` fields.
    # `--store-dir=` is a global flag and must come before the
    # subcommand; `--with-PROG=` flags belong to the subcommand and
    # must come after `v2-build` (otherwise cabal silently ignores
    # them and falls back to "near compiler" lookup, which fails for
    # cross GHCs that ship only prefixed binaries).
    cabalGlobalArgs="--store-dir=$storeDir"
    cabalCmdArgs="${crossWithFlags}${withProgFlags}"
    # Match v1's `-j` behaviour: cap GHC's per-module parallelism at
    # 4 even when nix gives us more cores.  Going much wider tends to
    # thrash memory on big modules (cardano-ledger templates,
    # plutus-core deriving, etc.) and slow the overall build.
    #
    # cabal-install 3.16's `setupHsBuildFlags` deliberately sets
    # `buildNumJobs = mempty` (with a "TODO" upstream), so neither
    # `cabal v2-build --jobs` nor `--ghc-options=-jN` actually
    # parallelise per-module compilation: `--jobs` is package-level
    # only, and `--ghc-options=-jN` would land in
    # `pkgHashConfigureOptions` and break plan-nix UnitId matching.
    # Our `setup-build-num-jobs-env.patch` reads
    # `HASKELLNIX_BUILD_NUM_JOBS` from the env at build time and feeds
    # it to `Setup build -jN`.  Build-phase flag, so UnitIds stay
    # stable.
    export HASKELLNIX_BUILD_NUM_JOBS=$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES))

    ${lib.optionalString (expectedPackage != null) ''
      # Dry-run the plan first and verify that cabal isn't planning
      # to build any package other than `expectedPackage`.  Anything
      # else is a dep cabal couldn't reuse from the composed
      # starting store — typically because some UnitId-hash input
      # (`--with-PROG=`, `package <n>` `ghc-options`,
      # `extra-(include|lib)-dirs`, dep UnitId, ...) diverged from
      # what produced that dep's slice.  Catching this early avoids
      # spending minutes rebuilding from source what should have
      # been a free store hit.
      echo "--- cabal v2-build --dry-run ${target} ---"
      cabal $cabalGlobalArgs v2-build $cabalCmdArgs --dry-run ${target} 2>&1 | tee plan.log

      # Parse the "In order, the following will/would be built:" block
      # — the per-package lines look like
      #   ` - foo-1.2.3 (lib) (requires build)`
      # — and split each `<pkg>-<ver>` token at the rightmost `-`
      # whose right-hand side starts with a digit.  Filter out
      # `expectedPackage` and any package in `allowedBuildToolPackages`
      # (transitive `build-tool-depends:` goals on cross, which the
      # solver always plans); whatever remains is a rebuild.
      expected_pkg=${lib.escapeShellArg expectedPackage}
      allowed_build_tools=${lib.escapeShellArg
        (lib.concatStringsSep " " allowedBuildToolPackages)}
      extras=$(awk -v expected="$expected_pkg" -v allowed="$allowed_build_tools" '
        BEGIN {
          n = split(allowed, a, " ")
          for (i = 1; i <= n; i++) allowed_set[a[i]] = 1
        }
        /^In order, the following / { capturing=1; next }
        capturing && /^$/ { capturing=0; next }
        capturing && /^ - / {
          pkg_ver = $2
          n = split(pkg_ver, parts, "-")
          for (i = n; i > 0; i--) if (parts[i] ~ /^[0-9]+(\.[0-9]+)*$/) break
          if (i <= 1) { name = pkg_ver }
          else {
            name = parts[1]
            for (j = 2; j < i; j++) name = name "-" parts[j]
          }
          if (name != expected && !(name in allowed_set)) print name
        }
      ' plan.log | sort -u)

      if [ -n "$extras" ]; then
        # Re-run cabal verbosely to capture the actual `Setup configure`
        # invocation cabal computed for each unexpected package.  The
        # `--cid=` and `--dependency=` lines on that command line ARE
        # cabal's UnitId hash inputs — comparing them with the
        # corresponding pre-installed unit's `.conf` makes the diverged
        # input visible at a glance.  We'd be running `cabal v2-build`
        # at this point anyway, so the only extra cost is the verbose
        # output.  (`|| true` because cabal will exit non-zero in the
        # presence of errors that the dev needs to see — we still want
        # to print the diagnostic.)
        echo "" >&2
        echo "[diagnostic] re-running cabal v2-build -v to capture setup" >&2
        echo "[diagnostic] configure args for the unexpected packages..." >&2
        cabal $cabalGlobalArgs v2-build $cabalCmdArgs -v ${target} > diag.log 2>&1 || true

        echo "" >&2
        echo "ERROR: cabal v2-build planned to build packages other than" >&2
        echo "$expected_pkg in this slice:" >&2
        printf '  - %s\n' $extras >&2
        echo "" >&2

        for pkg in $extras; do
          echo "==== $pkg ============================================" >&2

          # cabal's planned setup configure call.  Format:
          #   ["--working-dir=...","configure","--verbose=2",...]
          # Find the line that mentions tmp/src-N/<pkg>-<ver> (cabal
          # extracts each package into a unique src dir before calling
          # Setup configure), then split on `","` and print only the
          # hash-relevant args (`--cid=`, `--dependency=`,
          # `--flags=`, `--exact-configuration`, `--package-db=`).
          echo "" >&2
          echo "  cabal's Setup configure call (hash-relevant args):" >&2
          awk -v p="$pkg" '
            /^\[/ && $0 ~ "tmp/src-[0-9]+/" p "-[0-9]" {
              n = split($0, a, "\",\"")
              for (i = 1; i <= n; i++) {
                arg = a[i]
                gsub(/^\["/, "", arg)
                gsub(/"\]$/, "", arg)
                if (arg ~ /^--cid=/ \
                    || arg ~ /^--dependency=/ \
                    || arg ~ /^--flags?=/ \
                    || arg ~ /^-f / \
                    || arg ~ /^--package-db=/ \
                    || arg ~ /^--exact-configuration/ \
                    || arg ~ /^--ghc-option=/) {
                  print "    " arg
                }
              }
              exit
            }
          ' diag.log >&2

          # Pre-installed units in the starting store with the same
          # name.  cabal would have reused these if any of their
          # UnitIds matched what cabal computed above; the fact that
          # this branch fired means none did, so the diff between the
          # `--dependency=` list and each unit's `depends:` line is
          # the smoking gun.  Filter to symlink confs only — those are
          # the originals composed in via lndir.  Real-file confs at
          # this point come from the diagnostic v2-build run we just
          # made and would just clutter the listing.
          echo "" >&2
          echo "  Pre-installed unit(s) for $pkg in the starting store:" >&2
          found=0
          for conf in $ghcDir/package.db/*.conf; do
            [ -L "$conf" ] || continue
            pname=$(awk '$1=="name:"{print $2; exit}' "$conf")
            [ "$pname" = "$pkg" ] || continue
            found=1
            id=$(awk '$1=="id:"{print $2; exit}' "$conf")
            version=$(awk '$1=="version:"{print $2; exit}' "$conf")
            # `depends:` may span multiple lines (continuation indent)
            depends=$(awk '
              /^depends:/ { flag=1; sub(/^depends:[[:space:]]*/, ""); printf "%s ", $0; next }
              flag && /^[[:space:]]/ { printf "%s ", $0; next }
              flag { exit }
            ' "$conf" | xargs)
            echo "    id:       $id" >&2
            echo "    version:  $version" >&2
            echo "    depends:  $depends" >&2
            echo "    .conf:    $conf" >&2
            echo "" >&2
          done
          if [ "$found" = 0 ]; then
            echo "    (no pre-installed unit with this package name)" >&2
            echo "" >&2
          fi
        done

        echo "" >&2
        echo "A UnitId is a hash of cabal's Setup configure args, so the" >&2
        echo "ones that drift between slices are usually:" >&2
        echo "  * --dependency=<dep>=<dep-unit-id> — a transitive dep" >&2
        echo "    is resolving to a different UnitId, often because" >&2
        echo "    cabal in this slice picks a GHC-bundled" >&2
        echo "    (\"-inplace\") version while the dep's standalone" >&2
        echo "    slice picked a reinstalled version (or vice versa)." >&2
        echo "    Pin the version in cabal.project's constraints." >&2
        echo "  * --flags=... — flag assignment differs.  Check the" >&2
        echo "    \`package <n>\\n  flags:\` blocks emitted by" >&2
        echo "    \`comp-v2-builder.flagBlockFor\`." >&2
        echo "  * --ghc-option=... — \`package <n>\\n  ghc-options:\`" >&2
        echo "    blocks differ.  Check \`perPackageOptionOf\` and" >&2
        echo "    \`configure-args.nix\`'s plan-id-keyed entries." >&2
        exit 1
      fi
    ''}

    ${if dryRunOnly then ''
      # `dryRunOnly` mode: stop after planning so the diagnostic in
      # `installPhase` can compare cabal's `dist-newstyle/cache/plan.json`
      # against plan-nix without the cost (and the failure modes) of
      # the actual build.  Only used by the `.checkAgainstPlan`
      # sibling drv.
      echo "--- cabal v2-build --dry-run ${target} (dryRunOnly) ---"
      cabal $cabalGlobalArgs v2-build $cabalCmdArgs --dry-run ${target} 2>&1 | tee $buildRoot/build.log || true
    '' else ''
      # `cabal v2-build` accepts remote-package component targets
      # (e.g. `:pkg:foo:lib:sublibname`) as long as the package is
      # listed via `extra-packages:` in cabal.project AND its tarball
      # is in the slicing repo's index — both conditions are met by
      # comp-v2-builder for non-shim slices.
      echo "--- cabal v2-build ${target} ---"
      cabal $cabalGlobalArgs v2-build $cabalCmdArgs -v ${target} 2>&1 | tee $buildRoot/build.log
      ${lib.optionalString doHaddock ''
        echo "--- cabal v2-haddock ${target} ---"
        cabal $cabalGlobalArgs v2-haddock $cabalCmdArgs \
          --haddock-html --haddock-hyperlink-source --haddock-quickjump \
          ${target} 2>&1 | tee -a $buildRoot/build.log
        # Cabal `v2-haddock` installs the main library's html into
        # the unit-store `<unit>/share/doc/html/`, but for
        # sublibraries it only emits the html under the per-package
        # working dist dir at
        # `dist-newstyle/tmp/src-*/<pkg>-<ver>/dist/doc/html/<pkg>/<sublib>/`
        # — and then cabal wipes that tmp dir before the slice's
        # `installPhase` runs.  Capture the html here (still in the
        # buildPhase) and copy it into the sublib unit's
        # `share/doc/html/` so the doc slice matches the main-lib
        # layout for downstream consumers.
        tgt_pkg=$(printf '%s' ${lib.escapeShellArg target} | awk -F: '{print $3}')
        tgt_cname=$(printf '%s' ${lib.escapeShellArg target} | awk -F: '{print $5}')
        if [ "$tgt_cname" != "$tgt_pkg" ]; then
          sublib_html_src=$(find $buildRoot/project/dist-newstyle/tmp \
                              -path "*/dist/doc/html/$tgt_pkg/$tgt_cname" \
                              -type d -print -quit 2>/dev/null || true)
          if [ -n "$sublib_html_src" ]; then
            # The sublib's conf was just written by `cabal v2-build`'s
            # register step before `v2-haddock` ran; find its uid by
            # the cabal-mangled `z-<pkg>-z-<sublib>` name.
            for conf in $out/store/ghc-*/package.db/*.conf; do
              [ -e "$conf" ] || continue
              if [ "$(awk '/^name:/ {print $2; exit}' "$conf")" \
                   = "z-$tgt_pkg-z-$tgt_cname" ]; then
                target_uid=$(awk '/^id:/ {print $2; exit}' "$conf")
                ghc_dir=$(dirname "$(dirname "$conf")")
                target_html_dir="$ghc_dir/$target_uid/share/doc/html"
                mkdir -p "$target_html_dir"
                chmod -R u+w "$target_html_dir"
                cp -rL "$sublib_html_src"/. "$target_html_dir/"
                break
              fi
            done
          fi
        fi
      ''}
    ''}
  '';

  installPhase = if dryRunOnly then ''
    set -euo pipefail

    # `dryRunOnly` install: capture cabal's planned plan.json (and,
    # when `planNixJsonFile` was passed, diff each entry against
    # plan-nix).  Always exits 0 so the diagnostic remains
    # inspectable even when cabal's planning fails.
    mkdir -p $out
    cp $buildRoot/build.log $out/build.log 2>/dev/null || true

    # Capture the slice's cabal.project verbatim, plus every shim
    # `.cabal` file extracted from a `hs-nix-v2-shim-*.tar.gz` staged
    # in the project dir.  The shim's `build-depends` line carries
    # the transitive lib-dep version pins comp-v2-builder synthesised
    # — diffing it against plan-nix's resolved versions is often the
    # quickest way to see why the solver chose a different unit-id.
    if [ -f $buildRoot/project/cabal.project ]; then
      cp $buildRoot/project/cabal.project $out/cabal.project
    fi
    for shim_cabal in $buildRoot/project/*.tar.gz; do
      [ -f "$shim_cabal" ] || continue
      case "$shim_cabal" in
        */hs-nix-v2-shim-*.tar.gz)
          base=$(basename "$shim_cabal" .tar.gz)
          tar -xOzf "$shim_cabal" --wildcards '*/hs-nix-v2-shim.cabal' \
            > $out/$base.cabal 2>/dev/null \
            || tar -xOzf "$shim_cabal" "$base/hs-nix-v2-shim.cabal" \
              > $out/$base.cabal 2>/dev/null \
            || rm -f $out/$base.cabal
          ;;
      esac
    done

    actualPlan=$buildRoot/project/dist-newstyle/cache/plan.json
    if [ -f "$actualPlan" ]; then
      cp "$actualPlan" $out/plan.json
    fi

    ${lib.optionalString (planNixJsonFile != null) ''
      cp ${planNixJsonFile} $out/plan-nix.json
      diffReport=$out/diff-report.txt
      : > "$diffReport"

      if [ ! -f "$actualPlan" ]; then
        echo "(no dist-newstyle/cache/plan.json — cabal failed to plan)" \
          > "$diffReport"
        exit 0
      fi

      # For each `configured` entry in the slice's plan.json, find a
      # matching entry in plan-nix (by id, otherwise by pkg-name +
      # component-name + component-type) and emit a `jq -S | diff`
      # with full file context.  Pre-existing / cabal-shim entries
      # are skipped (no comparable plan-nix entry).
      jq -c '.["install-plan"] | .[] | select(.type == "configured")' \
        "$actualPlan" \
      | while IFS= read -r actualEntry; do
          actualId=$(echo "$actualEntry" | jq -r '.id')
          pkgName=$(echo "$actualEntry" | jq -r '."pkg-name" // empty')
          # Find plan-nix entry: id match first, otherwise by
          # pkg-name + component-name + component-type.
          expectedEntry=$(jq --arg id "$actualId" --argjson actual "$actualEntry" '
            (map(select(.id == $id)) | .[0]) //
            (map(select(
                ."pkg-name"   == ($actual."pkg-name") and
                ."pkg-version"== ($actual."pkg-version") and
                (."component-name" // null) == ($actual."component-name" // null) and
                (."component-type" // null) == ($actual."component-type" // null)
            )) | .[0]) //
            empty
          ' ${planNixJsonFile})
          {
            echo "=========================================================="
            echo "$pkgName  (actual id: $actualId)"
            echo "=========================================================="
            if [ -z "$expectedEntry" ]; then
              echo "(no plan-nix entry for $pkgName / $actualId)"
              echo
              echo "actual:"
              echo "$actualEntry" | jq -S .
              continue
            fi
            diff -U 1000000 \
              <(echo "$expectedEntry" | jq -S .) \
              <(echo "$actualEntry"   | jq -S .) || true
            echo
          } >> "$diffReport"
        done
      echo "Plan diff report: $diffReport" >&2
    ''}

    exit 0
  '' else ''
    set -euo pipefail

    # `$out/store` is the cabal store dir we built into (composed
    # via `lndir` from the dep slices, with cabal's new units
    # written as real files alongside the symlinks).  Real files
    # are ours; symlinks are dep-slice content that we just leave
    # in place so downstream consumers can follow them back to the
    # original dep slices.
    ghcDir=$(ls -d $out/store/ghc-* 2>/dev/null | head -n1 || true)
    if [ -z "$ghcDir" ]; then
      rm -rf $out/store
      mkdir -p $out
    else
      : > $buildRoot/captured-unit-ids

      # ---- Walk new confs --------------------------------------
      # Real files (not symlinks) are units cabal installed in this
      # slice.  Record them as captured.
      for conf in $ghcDir/package.db/*.conf; do
        [ -e "$conf" ] || continue
        [ -L "$conf" ] && continue        # dep-slice symlink, not ours
        base=$(basename $conf)
        unitId=''${base%.conf}
        echo "$unitId" >> $buildRoot/captured-unit-ids
        echo "captured store unit: $unitId"
      done

      # ---- Walk new unit dirs (incl. bin-only) ------------------
      # `lndir` materialised dep-slice unit dirs as real
      # directories full of symlinked files.  A "new" unit dir is
      # one that didn't exist before cabal ran (recorded in the
      # `unitdirs-before` snapshot).  Record bin-only units (exe /
      # test / bench, no .conf in package.db) as captured.
      for unitDir in $ghcDir/*/; do
        [ -d "$unitDir" ] || continue
        unitId=$(basename "$unitDir")
        case "$unitId" in
          lib|package.db|incoming) continue ;;
        esac
        if grep -qx -- "$unitId" $buildRoot/unitdirs-before 2>/dev/null; then
          continue
        fi
        if [ ! -f $ghcDir/package.db/$unitId.conf ]; then
          echo "$unitId" >> $buildRoot/captured-unit-ids
          echo "captured store unit: $unitId (bin-only)"
        fi
      done

      # ---- Drop cabal's `incoming/` staging area ---------------
      # `incoming/` is cabal's per-build install staging dir and
      # has nothing useful for downstream consumers.  Leaving it in
      # `$out` would lndir into the next slice's compose with the
      # read-only /nix/store mode bits, blocking cabal there from
      # creating its own `incoming/<unit>.lock` files.  Cabal
      # leaves the dir mode 555 after install, so chmod it
      # writable before rm — on darwin's sandbox even root
      # otherwise can't unlink inside it.
      if [ -d "$ghcDir/incoming" ]; then
        chmod -R u+w "$ghcDir/incoming" 2>/dev/null || true
        rm -rf "$ghcDir/incoming"
      fi

      if [ -z "$(ls $ghcDir/package.db/ 2>/dev/null)" ] \
         && [ "$(ls $ghcDir/ 2>/dev/null | wc -l)" -le 1 ]; then
        rm -rf $out/store
      else
        # Cabal bakes the absolute store-dir path into conf files
        # for the units it just installed.  Rewrite to ''${pkgroot}
        # so the conf is relocatable when downstream slices
        # re-compose it.  We only touch real (non-symlink) confs —
        # dep-slice confs were already rewritten by their producing
        # slice.
        storePrefix="$out/store/$(basename $ghcDir)"
        for conf in $ghcDir/package.db/*.conf; do
          [ -e "$conf" ] || continue
          [ -L "$conf" ] && continue
          sed -i "s|$storePrefix|\''${pkgroot}|g" $conf
          ${lib.optionalString (confLibraryDirs != []) ''
            # Append the slice's foreign-lib paths (from
            # `component.libs` — openssl, libsodium, ...) to the
            # .conf's `library-dirs:` / `library-dirs-static:` /
            # `dynamic-library-dirs:` fields.  GHC's runtime linker
            # uses these to dlopen each `extra-libraries:` entry
            # (e.g. `extra-libraries: ssl crypto` → `libssl.so`),
            # which would otherwise fail with `cannot open shared
            # object file` under `-fexternal-interpreter` because
            # neither cabal nor `ghc-iserv` carries the dirs in
            # its RPATH.  v1 baked these into the .conf via
            # `make-config-files.nix:flagsAndConfig "extra-lib-dirs"`;
            # in v2 we post-process after `cabal v2-build` registers
            # the unit so the unit-id stays equal to plan-nix's
            # (passing `--extra-lib-dirs` to `Setup configure`
            # would land in `pkgHashExtraLibDirs` and fork the
            # hash from plan-nix).
            for field in library-dirs library-dirs-static dynamic-library-dirs; do
              ${lib.concatMapStrings (p: ''
                sed -i "/^$field:/a\\    ${p}" $conf
              '') confLibraryDirs}
            done
          ''}
        done
        ${ghcPkgBin} --package-db=$ghcDir/package.db recache
      fi
    fi

    # Also expose dist-newstyle so callers can grab exes etc.
    if [ -d $buildRoot/project/dist-newstyle ]; then
      cp -r $buildRoot/project/dist-newstyle $out/dist-newstyle
    fi

    # `cabal v2-haddock` writes html into the haddocked unit's
    # `$out/store/<ghc>/<unit-id>/share/doc/html/` — the same
    # cabal-store layout the rest of the slice's `$out/store/`
    # already uses, so the doc slice can be `lndir`'d into
    # `~/.cabal/store/` without reshaping.  Cross-package
    # hyperlinks in the generated html are *absolute*
    # `file:///nix/store/<doc-slice>/store/<ghc>/<unit-id>/share/doc/html/...`
    # urls (haddock embeds the build-time `--read-interface=` path),
    # so they resolve back into the doc slice's own `$out/store/`
    # tree on the same host.  We deliberately don't synthesise a
    # stable `share/doc/html` symlink elsewhere — there is nothing
    # to gain by hiding the unit-id.

    ${lib.optionalString doHaddock ''
      # cabal v2-haddock emits html into every unit it touches —
      # the target plus every dep it haddock'd to satisfy
      # `--read-interface`.  We only want the target's html in
      # this slice's $out/store; deps' html lives in the deps'
      # own `.doc` slices.  Identify the target unit by the
      # conf's `name:` field — main libs use the plain pkg name,
      # sublibs use the cabal-mangled `z-<pkg>-z-<sublib>` form
      # — and strip `share/doc/` from every other unit dir.
      targetSpec=${lib.escapeShellArg target}
      tgt_pkg=$(printf '%s' "$targetSpec" | awk -F: '{print $3}')
      tgt_cname=$(printf '%s' "$targetSpec" | awk -F: '{print $5}')
      if [ "$tgt_cname" = "$tgt_pkg" ]; then
        expected_name="$tgt_pkg"
      else
        expected_name="z-$tgt_pkg-z-$tgt_cname"
      fi
      # Only consider confs this slice wrote — dep-slice confs are
      # lndir'd in as symlinks (see the `captured store unit` loop
      # near line 1004 that uses the same check).  Without this,
      # when a doc slice has both the base (non-doc) main-lib
      # composed in via depSlices AND its own doc-variant of the
      # same package, the first lexicographic match wins
      # (`5836f819` from the base slice vs `bb5ba586` this slice's
      # --cid).  Whichever it is, the loop's `break` may pick the
      # base-slice's conf — then we'd strip docs from this slice's
      # own --cid unit, which is exactly where cabal v2-haddock
      # installed them.
      target_uid=
      for conf in $out/store/ghc-*/package.db/*.conf; do
        [ -e "$conf" ] || continue
        [ -L "$conf" ] && continue
        name_in_conf=$(awk '/^name:/ {print $2; exit}' "$conf")
        if [ "$name_in_conf" = "$expected_name" ]; then
          target_uid=$(awk '/^id:/ {print $2; exit}' "$conf")
          break
        fi
      done
      if [ -n "$target_uid" ]; then
        for unit_dir in $out/store/ghc-*/*/; do
          uid=$(basename "$unit_dir")
          case "$uid" in
            package.db|incoming|lib) continue ;;
          esac
          if [ "$uid" != "$target_uid" ] && [ -d "$unit_dir/share/doc" ]; then
            chmod -R u+w "$unit_dir/share/doc" 2>/dev/null || true
            rm -rf "$unit_dir/share/doc"
          fi
        done
      else
        echo "WARN: doc slice could not locate the target unit" \
             "($expected_name) in the package db; not stripping" \
             "non-target haddocks." >&2
      fi
    ''}

    # Emit the UnitIds this slice's build produced, one per line.
    # Useful for:
    #   - diagnostics / downstream verification that the expected
    #     unit was actually built
    #   - distinguishing slices that share a cabal UnitId but were
    #     built with different ghc-options etc. (since cabal's
    #     UnitId doesn't hash everything that affects compiled
    #     bytes, two slices can collide here even when their nix
    #     store paths differ — emitting the UnitId makes the
    #     collision visible).
    if [ -s $buildRoot/captured-unit-ids ]; then
      cp $buildRoot/captured-unit-ids $out/unit-ids
    else
      : > $out/unit-ids
    fi

    # Emit the transitive set of dep slices this slice was composed
    # from.  Format: one slice store path per line, sorted, no
    # duplicates.  Downstream consumers list this slice as a *direct*
    # buildInput and read the file to bring in the indirect ones —
    # so transitive closure stays out of the nix-side eval.
    mkdir -p $out/nix-support
    {
      for dep in "''${pkgsHostTarget[@]}"; do
        if [ -d "$dep/store" ]; then
          echo "$dep"
          if [ -f "$dep/nix-support/transitive-deps" ]; then
            cat "$dep/nix-support/transitive-deps"
          fi
        fi
      done
    } | sort -u > $out/nix-support/transitive-deps

    ${lib.optionalString (v2Fragment != null) ''
      # --- v2 build-time-composition fragment (additive; STEP 1) -----
      # This package's own contribution, for a downstream slice to
      # compose the slicing repo + cabal.project at *build* time by
      # following pointer files — no Nix-side dep-graph walk.
      mkdir -p $out/repo-frag/package \
               $out/repo-frag/index/${v2Fragment.pkgName}/${v2Fragment.pkgVersion} \
               $out/nix-support/v2-frag
      # Source tarball (symlink — keeps the fragment a tiny set of links).
      ln -s ${v2Fragment.tarball} \
        $out/repo-frag/package/${v2Fragment.pkgName}-${v2Fragment.pkgVersion}.tar.gz
      # Index `.cabal`: the X-revised override when present, else the
      # revision-0 .cabal extracted from the tarball (byte-identical, so
      # cabal keeps `pkgHashPkgDescriptionHash` Nothing).
      ${if v2Fragment.cabalFile != null
        then "cp ${v2Fragment.cabalFile} $out/repo-frag/index/${v2Fragment.pkgName}/${v2Fragment.pkgVersion}/${v2Fragment.pkgName}.cabal"
        else ''
          tar -xOzf ${v2Fragment.tarball} \
            ${v2Fragment.pkgName}-${v2Fragment.pkgVersion}/${v2Fragment.pkgName}.cabal \
            > $out/repo-frag/index/${v2Fragment.pkgName}/${v2Fragment.pkgVersion}/${v2Fragment.pkgName}.cabal
        ''}
      # Per-package cabal.project blocks (emitted as separate files so a
      # consumer can group by block-type across the closure, matching the
      # current cabal.project byte-layout).  Written via `toFile` so the
      # multi-line block text needs no shell escaping.
      printf '%s' ${lib.escapeShellArg v2Fragment.pkgName}        > $out/nix-support/v2-frag/pkg-name
      cp ${builtins.toFile "flags"             v2Fragment.flagsBlock}            $out/nix-support/v2-frag/flags
      cp ${builtins.toFile "ghc-options"       v2Fragment.ghcOptionsBlock}       $out/nix-support/v2-frag/ghc-options
      cp ${builtins.toFile "configure-options" v2Fragment.configureOptionsBlock} $out/nix-support/v2-frag/configure-options
      cp ${builtins.toFile "program-options"   v2Fragment.programOptionsBlock}   $out/nix-support/v2-frag/program-options
      cp ${builtins.toFile "doc"               v2Fragment.docBlock}              $out/nix-support/v2-frag/doc
      cp ${builtins.toFile "extra-lib-dirs"    v2Fragment.extraLibDirsBlock}     $out/nix-support/v2-frag/extra-lib-dirs
      printf '%s' ${lib.escapeShellArg v2Fragment.constraintLine} > $out/nix-support/v2-frag/constraint
      cp ${builtins.toFile "sublib-seeds" (lib.concatMapStrings (s: s + "\n") v2Fragment.sublibSeeds)} \
        $out/nix-support/v2-frag/sublib-seeds
      cp ${builtins.toFile "setup-constraints" (lib.concatMapStrings (s: s + "\n") v2Fragment.setupConstraints)} \
        $out/nix-support/v2-frag/setup-constraints
      # Flattened lib-dep closure pointer (the constraints scope).
      # Seeds purely from this slice's DIRECT deps; transitivity is
      # accumulated here at build time (like `transitive-deps`):
      #   * each direct lib-dep slice — pinned (echoed) — plus its own
      #     flattened pointer (its lib deps + its build tools' closures);
      #   * each direct build-tool slice's pointer only (its lib closure
      #     is pinned; the exe package itself is not).
      # Following any dep's pointer therefore reaches every transitive
      # build tool's lib closure without a Nix-side closure walk.
      {
        ${lib.concatMapStrings (s: ''
          echo ${s}
          if [ -f ${s}/nix-support/lib-dep-slices ]; then
            cat ${s}/nix-support/lib-dep-slices
          fi
        '') v2Fragment.libDepSlices}
        ${lib.concatMapStrings (s: ''
          if [ -f ${s}/nix-support/lib-dep-slices ]; then
            cat ${s}/nix-support/lib-dep-slices
          fi
        '') v2Fragment.exeDepSlices}
        : # ensure the brace group is non-empty when there are no deps
      } | sort -u > $out/nix-support/lib-dep-slices
    ''}

    # Drop the package-db cache files from $out.  They're real files
    # `ghc-pkg recache` regenerated for this slice's specific composed
    # package.db, but downstream consumers compose a *different* set
    # of confs and run their own `recache`, so this cache is always
    # stale on arrival.  Leaving it in $out also means every dep
    # slice's `package.cache` collides at lndir time in the consumer
    # ("Keeping existing link to ..." spam scaling O(deps) per
    # consumer).  Cheaper to delete now and let downstream recreate.
    if [ -n "$ghcDir" ]; then
      rm -f $ghcDir/package.db/package.cache \
            $ghcDir/package.db/package.cache.lock
    fi

    # Clear dep-slice content out of $out/store now that this slice's
    # own unit is captured.  Downstream consumers don't read another
    # slice's $out/store directly — they walk
    # `nix-support/transitive-deps` and lndir each entry's $out/store
    # back into their own composed cabal-store.  So all the
    # lndir-composed dep symlinks (and the dep-slice unit dirs they
    # populated) are pure overhead in this slice's $out: fixupPhase
    # walks every entry, NAR serialisation `lstat`s every entry, and
    # Nix's reference scan follows them.  A deep dep graph leaves
    # 10k+ symlinks in here.
    #
    # When ghc links a shared library it bakes the composed-store
    # path into the `.so`'s DT_RUNPATH / Mach-O LC_RPATH (e.g.
    # `<this-slice>/store/.../<dep-uid>/lib`) so iserv-dyn / dyld
    # can resolve a dep's transitive `.so` deps at TH-eval time.
    # Plain "wipe the dep dirs" would leave the rpath pointing at
    # nothing — `shrink-rpath` (fixupPhase) drops the entry and
    # downstream dyld load fails with `No such file or directory`.
    # Rewrite our own ELF/Mach-O files' rpath entries to point at
    # the dep slice's actual /nix/store path (resolved through the
    # lndir symlink), then wipe the dep dirs.  shrink-rpath sees
    # entries pointing at real, populated dirs and keeps them.
    if [ -n "$ghcDir" ]; then
      # Identify the slice's own captured unit-ids — for the
      # typical known-uid case this is `expectedUnitId`; for the
      # unknown case (source-repo / local / Custom-Build packages)
      # it's the set cabal actually installed.
      ${if expectedUnitId == null then ''
        mapfile -t own_uids < $buildRoot/captured-unit-ids
      '' else ''
        own_uids=(${lib.escapeShellArg expectedUnitId})
      ''}

      # Rewrite RPATH entries that point into `$out/store/` (the
      # lndir-composed dep tree we're about to delete).  For each
      # such entry, resolve a symlinked file under it via
      # `readlink -f` and take that file's `dirname` — that's the
      # dep slice's own `/nix/store/<dep-slice>/store/.../<dep-uid>/lib`
      # path.  Entries that don't point into `$out/store/` (system
      # libs, $ORIGIN, ...) are left alone.
      ${let
        elfRewrite = ''
          rewrite_rpath_elf() {
            local f="$1"
            local old new entry resolved child tgt
            # `|| return 0` skips statically-linked ELF binaries
            # (e.g. musl `executable-static: True` exes — patchelf
            # bails out with "cannot find section `.dynamic`").
            old=$(patchelf --print-rpath "$f" 2>/dev/null) || return 0
            [ -n "$old" ] || return 0
            new=""
            local IFS=':'
            for entry in $old; do
              [ -n "$entry" ] || continue
              if [[ "$entry" == "$out/store/"* ]]; then
                resolved=""
                local found=0
                for child in "$entry"/*; do
                  [ -L "$child" ] || continue
                  tgt=$(readlink -f "$child")
                  [ -n "$tgt" ] || continue
                  resolved=$(dirname "$tgt"); found=1; break
                done
                if [ $found -eq 1 ]; then
                  new="''${new:+$new:}$resolved"
                fi
              else
                new="''${new:+$new:}$entry"
              fi
            done
            if [ "$old" != "$new" ]; then
              patchelf --set-rpath "$new" "$f"
            fi
          }
          rewrite_file_rpaths() { rewrite_rpath_elf "$1"; }
        '';
        machoRewrite = ''
          rewrite_rpath_macho() {
            local f="$1"
            # otool -l prints LC_RPATH commands in groups of three
            # lines; the `path <path> (offset N)` line carries the
            # rpath value.  Capture all of them, then replace each
            # one that points into $out/store via install_name_tool.
            # `|| return 0` skips non-Mach-O files (`.a` archives,
            # script wrappers etc.) without killing installPhase
            # under `set -e` + `pipefail`.
            local rpaths
            rpaths=$(otool -l "$f" 2>/dev/null \
              | awk '
                  /^Load command/ { in_rpath=0 }
                  /cmd LC_RPATH/   { in_rpath=1; next }
                  in_rpath && /path / { print $2 }
                ') || return 0
            [ -n "$rpaths" ] || return 0
            local old new resolved child tgt
            while IFS= read -r old; do
              [ -n "$old" ] || continue
              if [[ "$old" == "$out/store/"* ]]; then
                resolved=""
                local found=0
                shopt -s nullglob
                for child in "$old"/*; do
                  [ -L "$child" ] || continue
                  tgt=$(readlink -f "$child" 2>/dev/null) || continue
                  [ -n "$tgt" ] || continue
                  resolved=$(dirname "$tgt"); found=1; break
                done
                shopt -u nullglob
                if [ $found -eq 1 ] && [ "$old" != "$resolved" ]; then
                  chmod u+w "$f" 2>/dev/null || true
                  install_name_tool -rpath "$old" "$resolved" "$f" \
                    2>/dev/null || true
                else
                  chmod u+w "$f" 2>/dev/null || true
                  install_name_tool -delete_rpath "$old" "$f" \
                    2>/dev/null || true
                fi
              fi
            done <<< "$rpaths"
          }
          rewrite_file_rpaths() { rewrite_rpath_macho "$1"; }
        '';
      in if stdenv.hostPlatform.isLinux then elfRewrite
         else if stdenv.hostPlatform.isDarwin then machoRewrite
         else ''
           rewrite_file_rpaths() { :; }
         ''}

      # Restrict the walk to files that may carry an rpath: shared
      # libraries (`*.so*` on linux, `*.dylib*` on darwin) and any
      # file in a `bin/` dir (executables).  Without this, patchelf /
      # otool also get pointed at `.hi`, `.a`, `cabal-hash.txt` etc.
      # and would need defensive error handling per file.
      rpath_candidates=(
        \( -name '*.so' -o -name '*.so.*'
        -o -name '*.dylib' -o -name '*.dylib.*'
        -o -path '*/bin/*' \)
      )
      for own_uid in "''${own_uids[@]}"; do
        [ -n "$own_uid" ] || continue
        unit_dir="$ghcDir/$own_uid"
        [ -d "$unit_dir" ] || continue
        while IFS= read -r -d "" f; do
          rewrite_file_rpaths "$f"
        done < <(find "$unit_dir" -type f "''${rpath_candidates[@]}" -print0)
      done
      if [ -d "$ghcDir/lib" ]; then
        while IFS= read -r -d "" f; do
          rewrite_file_rpaths "$f"
        done < <(find "$ghcDir/lib" -type f "''${rpath_candidates[@]}" -print0)
      fi

      # Aggressive cleanup: move keepers aside, wipe $ghcDir, then
      # restore them.  Cheaper than walking the tree.  Keepers per
      # captured unit:
      #   * `$ghcDir/<uid>/`             — per-unit lib/share/etc.
      #   * `$ghcDir/package.db/<uid>.conf` — pkg-db entry
      #   * `$ghcDir/lib/libHS<uid>-*.{dylib,so,a}` — the flat
      #     shared lib cabal puts in `$ghcDir/lib/` alongside the
      #     per-unit dir.  Without this, downstream consumers
      #     can't find the dylib at link time.
      if [ ''${#own_uids[@]} -gt 0 ] \
         && ! { [ ''${#own_uids[@]} -eq 1 ] && [ -z "''${own_uids[0]}" ]; }; then
        side=$buildRoot/keep
        mkdir -p "$side/package.db" "$side/lib"
        shopt -s nullglob
        for keep in "''${own_uids[@]}"; do
          [ -n "$keep" ] || continue
          if [ -e "$ghcDir/$keep" ]; then
            mv "$ghcDir/$keep" "$side/$keep"
          fi
          if [ -e "$ghcDir/package.db/$keep.conf" ]; then
            mv "$ghcDir/package.db/$keep.conf" "$side/package.db/$keep.conf"
          fi
          for f in "$ghcDir/lib/libHS$keep"-*; do
            mv "$f" "$side/lib/$(basename "$f")"
          done
        done
        shopt -u nullglob
        rm -rf "$ghcDir"
        mkdir -p "$ghcDir/package.db"
        shopt -s nullglob
        for keep in "''${own_uids[@]}"; do
          [ -n "$keep" ] || continue
          if [ -e "$side/$keep" ]; then
            mv "$side/$keep" "$ghcDir/$keep"
          fi
          if [ -e "$side/package.db/$keep.conf" ]; then
            mv "$side/package.db/$keep.conf" "$ghcDir/package.db/$keep.conf"
          fi
        done
        kept_libs=("$side/lib/libHS"*)
        if [ ''${#kept_libs[@]} -gt 0 ]; then
          mkdir -p "$ghcDir/lib"
          for f in "''${kept_libs[@]}"; do
            mv "$f" "$ghcDir/lib/$(basename "$f")"
          done
        fi
        shopt -u nullglob
      fi
      # When captured_uids is empty (cabal short-circuited "Up to
      # date" — a dep slice's lndir composition supplied
      # everything), leave $ghcDir alone.  `kindSpecificInstallPhase`
      # needs to find `bin/<exeName>` somewhere in `$out/store` for
      # exe / test / bench slices, and the only place it lives is
      # the lndir-composed dep slice unit dir.
    fi

    ${if expectedUnitId == null then ''
      # No specific unit-id to match against (source-repo / local
      # packages, cross builds where the cross plan's network uid
      # legitimately diverges from what real cross-cabal computes
      # due to `--with-PROG` flags entering pkgHashProgramArgs,
      # custom-build packages whose plan-nix entry spans multiple
      # cabal-side units, etc.).  Verify:
      #   * exactly ONE captured unit has pkg-name == expectedPackage
      #     (the slice's target — missing it means cabal built
      #     something other than what we asked for);
      #   * any OTHER captured units have a pkg-name in
      #     `allowedBuildToolPackages` (transitive build-tools on
      #     cross — normally none, since cabal recognises the
      #     composed cross `targetSlice` by matching uid, but if a
      #     uid mismatch slips through we accept the captured tool
      #     unit rather than fail outright; downstream slices reuse
      #     it through `composeStore`).
      expected_pkg=${lib.escapeShellArg expectedPackage}
      allowed_build_tools=${lib.escapeShellArg
        (lib.concatStringsSep " " allowedBuildToolPackages)}
      actual_uids=$(sort -u $buildRoot/captured-unit-ids)

      # Map each captured unit-id to its package name.  Prefer the
      # `.conf` file (lib units) for the authoritative pkg name —
      # cabal's OS-prefix patch shortens unit-id prefixes (e.g.
      # `bytrdr-1.0.4-...` for `byteorder`), so parsing the uid
      # alone misclassifies them.  For sublibs cabal records:
      #   name:         z-<pkg>-z-<sublib>
      #   package-name: <pkg>
      #   lib-name:     <sublib>
      # — prefer `package-name:` so a sublib slice matches the real
      # package, not the z-encoded one.  Fall back to uid parsing
      # for bin-only units (exe / test / bench — no .conf).
      planJson="$buildRoot/project/dist-newstyle/cache/plan.json"
      uid_to_pkg() {
        while IFS= read -r uid; do
          conf="$ghcDir/package.db/$uid.conf"
          if [ -f "$conf" ]; then
            name=$(awk '/^package-name:/ {print $2; exit}' "$conf")
            if [ -z "$name" ]; then
              name=$(awk '/^name:/ {print $2; exit}' "$conf")
            fi
            if [ -n "$name" ]; then
              echo "$name	$uid"
              continue
            fi
          fi
          # Bin-only (exe / test / bench) units have no `.conf` —
          # look the uid up in cabal's own `plan.json` instead.
          # Falling back to parsing the uid string would misclassify
          # OS-prefix-shortened names (e.g. `bckpck-...` for
          # `backpack`).
          if [ -f "$planJson" ]; then
            name=$(jq -r --arg id "$uid" '.["install-plan"][] | select(.id == $id) | .["pkg-name"]' "$planJson" 2>/dev/null | head -n1)
            if [ -n "$name" ] && [ "$name" != "null" ]; then
              echo "$name	$uid"
              continue
            fi
          fi
          # Last resort: parse uid (works for unprefixed exe uids
          # — typically build-tools whose pkg-name appears in
          # `allowedBuildToolPackages`).
          echo "$uid" | awk '{
            n = split($1, parts, "-")
            for (i = n; i > 0; i--) if (parts[i] ~ /^[0-9]+(\.[0-9]+)*$/) break
            if (i <= 1) { name = $1 }
            else {
              name = parts[1]
              for (j = 2; j < i; j++) name = name "-" parts[j]
            }
            print name "\t" $1
          }'
        done
      }

      pairs=$(printf '%s\n' "$actual_uids" | uid_to_pkg)
      # `+`-suffixed unit-ids are cabal backpack instantiations of
      # an indefinite library — byproducts cabal emits inline when
      # something downstream needs the instantiated form (e.g. the
      # exe slice materialises the instantiated `consumer` because
      # the indefinite consumer's slice has no compiled artifacts
      # to compose in).  v1 builds each instantiation as its own
      # derivation (lib/default.nix:369's
      # `components.library.override { inherit instantiations; }`);
      # v2 doesn't yet have a slice-per-instantiation, so these
      # ride along inside the consuming slice's $out.  Exclude
      # them from the target-unit count so the check sees the
      # slice's own target only.
      target_count=$(printf '%s\n' "$pairs" \
        | awk -v e="$expected_pkg" '$1 == e && $2 !~ /\+/' \
        | wc -l)
      unexpected=$(printf '%s\n' "$pairs" \
        | awk -v expected="$expected_pkg" -v allowed="$allowed_build_tools" '
            BEGIN {
              n = split(allowed, a, " ")
              for (i = 1; i <= n; i++) allowed_set[a[i]] = 1
            }
            $1 != expected && !($1 in allowed_set) { print $2 }
          ')

      # `target_count > 1` indicates cabal split the target into
      # multiple installed units within a single slice — that's
      # the breakage we want to catch.  `target_count == 0` is OK:
      # cabal silently produces no `.conf` for indefinite backpack
      # libraries (the slice's target is a sublib with unfilled
      # `signatures:` — no compiled artifacts to install until
      # something instantiates it), so an empty $out is the
      # correct outcome for that case.
      if [ "$target_count" -gt 1 ]; then
        echo "" >&2
        echo "ERROR: slice captured $target_count non-instantiated units for the target package '$expected_pkg'; expected 0 or 1." >&2
        echo "" >&2
        echo "  Captured unit-ids:" >&2
        printf '%s\n' "$actual_uids" | sed 's/^/    /' >&2
        exit 1
      fi
      if [ -n "$unexpected" ]; then
        echo "" >&2
        echo "ERROR: slice captured units for packages other than '$expected_pkg'" >&2
        echo "       and the allowed transitive build-tools:" >&2
        echo "" >&2
        echo "  Unexpected unit-ids:" >&2
        printf '%s\n' "$unexpected" | sed 's/^/    /' >&2
        echo "" >&2
        echo "  All captured unit-ids:" >&2
        printf '%s\n' "$actual_uids" | sed 's/^/    /' >&2
        exit 1
      fi
    '' else ''
      # The slice must produce exactly one unit-id: the plan-id
      # plan-nix recorded for this component.  Any divergence is a
      # fail:
      #   * Missing — cabal's slice solver picked different inputs
      #     than the project plan, so the unit cabal actually built
      #     is a different unit-id.  Typically the slice solver
      #     chose a GHC-bundled `-inplace` dep where plan-nix had a
      #     reinstalled one.
      #   * Extra — cabal built more than this slice's component.
      #     The most common case is an exe slice where cabal
      #     accidentally rebuilt the package's library alongside
      #     the exe instead of reusing the pre-installed lib slice.
      #     The lib's slice is what downstream consumers compose,
      #     so a second copy with a different unit-id would
      #     silently fork dep hashes.
      expected_uid=${lib.escapeShellArg expectedUnitId}
      actual_uids=$(sort -u $buildRoot/captured-unit-ids)
      if printf '%s\n' "$actual_uids" | grep -Fxq -- "$expected_uid"; then
        missing=""
      else
        missing="$expected_uid"
      fi
      unexpected=$(comm -13 \
        <(printf '%s\n' "$expected_uid") \
        <(printf '%s\n' "$actual_uids"))
      if [ -n "$missing" ] || [ -n "$unexpected" ]; then
        echo "" >&2
        echo "ERROR: slice produced unit-ids that don't match plan-nix.json:" >&2
        echo "" >&2
        echo "  Expected (from plan-nix):" >&2
        echo "    $expected_uid" >&2
        echo "" >&2
        echo "  Actual (in this slice's \$out/store):" >&2
        printf '%s\n' "$actual_uids" | sed 's/^/    /' >&2
        if [ -n "$missing" ]; then
          echo "" >&2
          echo "  Missing (in plan but not produced):" >&2
          echo "    $missing" >&2
        fi
        if [ -n "$unexpected" ]; then
          echo "" >&2
          echo "  Unexpected (produced but not expected):" >&2
          printf '%s\n' "$unexpected" | sed 's/^/    /' >&2
        fi

        ${lib.optionalString (expectedPlanEntries != null) ''
          # For each `unexpected` unit-id, diff the plan-nix entry
          # (passed in via `expectedPlanEntries`) against the entry
          # cabal-install just produced (in
          # `dist-newstyle/cache/plan.json`).  Both are pretty-printed
          # with `jq -S` and diffed with full-file context so the
          # diverging field (compiler-id, flags, dep unit-ids, ...) is
          # visible against the rest of the entry.
          actualPlan=$buildRoot/project/dist-newstyle/cache/plan.json
          expectedPlanFile=${pkgsBuildBuild.writeText "expected-plan-entries.json"
                              (builtins.toJSON expectedPlanEntries)}
          if [ -n "$unexpected" ] && [ -f "$actualPlan" ]; then
            echo "" >&2
            echo "==== plan-entry diff: plan-nix vs slice's plan.json ====" >&2
            for actualId in $unexpected; do
              echo "" >&2
              echo "  ---- expected $expected_uid  vs  actual $actualId ----" >&2
              expectedJson=$(jq --arg id "$expected_uid" '.[$id]' $expectedPlanFile)
              actualJson=$(jq --arg id "$actualId" \
                '.["install-plan"] | map(select(.id == $id)) | .[0]' $actualPlan)
              if [ "$expectedJson" = "null" ]; then
                echo "    (no plan-nix entry for $expected_uid)" >&2
                continue
              fi
              if [ "$actualJson" = "null" ]; then
                echo "    (no entry for $actualId in dist-newstyle/cache/plan.json)" >&2
                continue
              fi
              # `--sort-keys` (-S) recursively sorts object keys so a
              # field that's emitted in a different position in
              # plan-nix vs. cabal-install's plan.json doesn't show
              # up as a "difference" in the diff.
              diff -U 1000000 \
                <(echo "$expectedJson" | jq -S .) \
                <(echo "$actualJson"   | jq -S .) >&2 || true
            done
          fi
        ''}

        # The actual `cabal v2-build` was run with `-v` and its output
        # captured to `$buildRoot/build.log`.  The Setup configure call
        # cabal made for each unit appears there as a single JSON-array
        # line; the `--cid=<unit-id>` arg identifies the unit.  Dump
        # them so we can diff against plan-nix's `configure-args`.
        echo "" >&2
        echo "==== Setup configure calls cabal made for this slice ====" >&2
        grep -E '^\[".*configure' $buildRoot/build.log >&2 || true

        echo "" >&2
        echo "Unit-id hashes capture the full set of \`Setup configure\`" >&2
        echo "args (compiler, flags, dep unit-ids, program args, ...).  A" >&2
        echo "divergence usually means the slice's solver picked a" >&2
        echo "different dep version or a different unit-id of the same" >&2
        echo "version (e.g. \`-inplace\` vs reinstalled).  Pin the" >&2
        echo "transitive closure via cabal.project \`constraints:\` lines" >&2
        echo "so the slice solver matches the project plan." >&2
        exit 1
      fi
    ''}
  '';
})
