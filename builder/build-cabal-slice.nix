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
#   $out/dist-newstyle/                              -- full dist-newstyle tree
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
{ stdenv, lib, ghc, cabal-install, pkgsBuildBuild, buildPackages }@outerArgs:

let outerGhc = ghc; in

{ name
, depSlices ? []
, localRepo ? null           # derivation with <pkg>-<ver>.tar.gz files
, preBuild                   # stage sources, write cabal.project, cd into project dir
, target ? "all"
, passthru ? {}              # extra attrs to attach to the derivation
, extraBuildInputs ? []      # e.g. component.libs / frameworks — things ld needs at link time
, extraNativeBuildInputs ? [] # e.g. build-tools — tools on PATH at build time
, propagated ? []            # things to put in `propagatedBuildInputs`
                             # so downstream slices (using this slice as a
                             # buildInput) inherit them automatically via
                             # stdenv's propagation chain.
, ghc ? outerGhc             # caller may override (e.g. swap in a wrapped
                             # ghc for cross-TH); defaults to the outer `ghc`.
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
  ghcShim = pkgsBuildBuild.runCommand "${ghc.name}-shim" {
    preferLocalBuild = true;
  } ''
    mkdir -p $out/bin
    for f in ${ghc}/bin/*; do
      base=$(basename "$f")
      ln -s "$f" "$out/bin/$base"
      case "$base" in
        ${targetPrefix}*)
          unprefixed=''${base#${targetPrefix}}
          [ -e "$out/bin/$unprefixed" ] || ln -s "$f" "$out/bin/$unprefixed"
          ;;
      esac
    done
  '';
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
  name = "cabal-slice-${name}";
  # GHC / hsc2hs / cabal write/read source files; without a UTF-8
  # locale they fall back to the C encoding and crash on non-ASCII
  # input (e.g. `commitBuffer: invalid argument (cannot encode
  # character '\194')` when hsc2hs hits a degree symbol).  Mirrors
  # `comp-builder.nix:459-461,503`.
  LANG = "en_US.UTF-8";
  LC_ALL = "en_US.UTF-8";
} // lib.optionalAttrs (stdenv.buildPlatform.libc == "glibc") {
  LOCALE_ARCHIVE = "${buildPackages.glibcLocales}/lib/locale/locale-archive";
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
                        pkgsBuildBuild.jq ]
    ++ lib.optional stdenv.hostPlatform.isGhcjs pkgsBuildBuild.nodejs
    ++ extraNativeBuildInputs;
  # `depSlices` go in `buildInputs` so stdenv exposes them via
  # `pkgsHostTarget` (a bash array of host-platform inputs) — the
  # buildPhase walks that array to find each slice's `store/`
  # subdir and `lndir` it into our composed cabal store.  Same
  # pattern as v1's `make-config-files.nix:102-114`, where the
  # array is walked to find `package.conf.d` dirs.  Slices have
  # no `lib/` / `include/` at the $out level so stdenv's normal
  # NIX_LDFLAGS / NIX_CFLAGS injection finds nothing to add — the
  # presence in buildInputs is purely for propagation + iteration.
  buildInputs = extraBuildInputs ++ depSlices;
  propagatedBuildInputs = propagated;
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

    # Snapshot existing unit filenames so we can identify new ones later.
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
      ( cd $ghcDir && ls -d */ 2>/dev/null | sed 's|/$||' ) \
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

    ${lib.optionalString (localRepo != null) ''
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
    cabalCmdArgs="${crossWithFlags}"

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
      # `expectedPackage` directly; whatever remains is a rebuild.
      expected_pkg=${lib.escapeShellArg expectedPackage}
      extras=$(awk -v expected="$expected_pkg" '
        /^In order, the following / { capturing=1; next }
        capturing && /^$/ { capturing=0; next }
        capturing && /^ - / {
          pkg_ver = $2
          n = split(pkg_ver, parts, "-")
          for (i = n; i > 0; i--) if (parts[i] ~ /^[0-9]/) break
          if (i <= 1) { name = pkg_ver }
          else {
            name = parts[1]
            for (j = 2; j < i; j++) name = name "-" parts[j]
          }
          if (name != expected) print name
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
        if grep -qx "$unitId" $buildRoot/unitdirs-before 2>/dev/null; then
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
        done
        ${ghcPkgBin} --package-db=$ghcDir/package.db recache
      fi
    fi

    # Also expose dist-newstyle so callers can grab exes etc.
    if [ -d $buildRoot/project/dist-newstyle ]; then
      cp -r $buildRoot/project/dist-newstyle $out/dist-newstyle
    fi

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

    ${lib.optionalString (expectedUnitId != null) ''
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
      if printf '%s\n' "$actual_uids" | grep -Fxq "$expected_uid"; then
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
