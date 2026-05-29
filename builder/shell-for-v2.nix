# v2 counterpart to shell-for.nix.
#
# Unlike the v1 shell — which pre-populates a ghc-pkg database for GHC
# directly — the v2 shell pre-populates the user's cabal store
# (typically `~/.cabal/store/ghc-X.Y.Z`).  That's where cabal v2-build
# looks for already-installed units, so once the store has the deps the
# user works on, plain `cabal build` inside the shell reuses them
# instead of rebuilding from source.
#
# The shell hook handles store composition carefully:
#   * Items are installed via `ln -s` / `lndir` so the user's cabal
#     store contains symlinks pointing at the composed store in
#     /nix/store rather than full copies.  A nix GC root is added so
#     `nix-collect-garbage` won't remove the underlying paths.
#   * If a target unit is missing (or a previously-synced symlink is
#     now broken because its target was GC'd), install it.
#   * If it exists and byte-matches the source, no-op.
#   * If it exists and differs, refuse to overwrite; running
#     `haskell-nix-cabal-store-sync --force` replaces them.
#
# This keeps the user's store in a consistent state for their other
# projects while still letting them iterate on this one.
{ lib, stdenv, pkgs, runCommand, mkShell, hsPkgs, haskellLib, ghc, haskell-nix
, compiler, composeStore, makeGhcShim, cabalProjectLocal ? null }:

{ # Same shape as shellFor's `packages`: packages the user works on.
  # Their *dependencies* are composed into the shell's cabal store;
  # the packages themselves are not.
  # Default to *project* packages (working-tree), not all *local*
  # packages.  `selectLocalPackages` also includes
  # source-repository-packages, but SRPs are pinned upstream code
  # the user doesn't iterate on — we want them pre-built into the
  # cabal store, not excluded from it.
  packages ? ps: builtins.attrValues (haskellLib.selectProjectPackages ps)
  # Extra tools built from hackage and placed on PATH.  Same shape as
  # shellFor's `tools` argument: each value is a versionOrMod passed
  # to `haskell-nix.tool`.
  #   tools = { cabal = "3.16.1.0"; hlint = { version = "3.6"; }; }
, tools ? {}
  # If true, put `hoogle` on PATH (via haskell-nix.tool).  A hoogle
  # database is *not* pre-built — v2 doesn't produce haddocks the way
  # v1 does, so the user runs `hoogle generate` themselves when they
  # want a local db.
, withHoogle ? false
  # Accepted for API parity with shellFor.  v2 slices don't currently
  # produce haddocks, so setting this has no effect beyond documenting
  # intent; the flag is silently ignored when the haddock machinery
  # would otherwise be wired in.
, withHaddock ? withHoogle
  # Put `build-tool-depends` for this shell's components on PATH.  If
  # `allToolDeps = true` (the default, matching v1's shellFor), the
  # tools come from *every* component of every hsPkg — so cabal can
  # fall back to building any hackage dep inside the shell without
  # missing a build tool.  When `false`, only the selected packages'
  # build tools are included (leaner, but `cabal build` on a dep that
  # needs e.g. `alex` will have to find or build it itself).
, allToolDeps ? true
  # How the shell's pre-built library deps are made visible to tools
  # the user runs.  One of:
  #   "cabal-store" (default) — copy the composed store into
  #     `~/.cabal/store/<ghc>-inplace/` via a shellHook (or the
  #     explicit `haskell-nix-cabal-store-sync` command).  `cabal
  #     v2-build` then reuses the units.  The shell's `ghc` is
  #     plain — `runghc` / `ghci` won't see the deps.
  #   "ghc-pkg" — skip the cabal-store sync; instead wrap `ghc`
  #     with GHC_ENVIRONMENT pointing at the composed store's
  #     package.db, so `ghc`/`ghci`/`runghc`/`ghc-pkg` see every
  #     dep directly.  `cabal v2-build` still reuses them (it
  #     queries the wrapped ghc-pkg to discover installed units),
  #     but no files are written outside the shell.
  # We surface the dep closure via exactly one of these paths to
  # avoid confusion (e.g. stale package.db vs up-to-date store).
, exposePackagesVia ? "cabal-store"
  # Other derivations whose buildInputs / nativeBuildInputs / shellHook
  # should be merged into this shell (e.g. cross-compilation shells).
, inputsFrom ? []
  # Pre-built derivations to add directly (kept for parity with the
  # pre-tools API).
, additionalTools ? []
  # Extra build inputs (C libs, pkg-config deps, etc.) to surface in
  # the shell alongside the composed cabal store.
, buildInputs ? []
, nativeBuildInputs ? []
, shellHook ? ""
, ...
}@args:

let
  selectedPackages = packages hsPkgs;

  # All components of the selected packages (library, sublibs, exes,
  # tests, benchmarks).  From these we derive the set of external
  # library deps that should land in the cabal store.
  selectedComps = lib.concatMap haskellLib.getAllComponents selectedPackages;

  # Mirrors v1's `selectedConfigs`: include each non-Simple
  # package's `setup.config` so that setup-depends (e.g. Custom
  # build-type packages' `conduit` / `Cabal` / etc.) end up in the
  # shell env.  Without this, `runghc script.hs` inside the shell
  # can't see conduit, because pkg's library `build-depends` lists
  # only `base`.
  selectedSetupConfigs = lib.filter (c: c != null)
    (map (p: p.setup.config or null)
      (lib.filter (p: (p.buildType or "Simple") != "Simple") selectedPackages));

  # Resolve a `component.depends` entry to its haskell.nix package
  # value (`hsPkgs.<name>`).  A dep is either:
  #   * a package record (has `.components`) — return as-is, OR
  #   * a sublib slice (has `.passthru.identifier.name`) — look up
  #     the package the slice comes from in `hsPkgs`.
  depPkg = d:
    if d ? components then d
    else hsPkgs.${d.passthru.identifier.name or ""} or null;

  # Build-tool exe (`executableToolDepends` entry) → its package.
  exePkg = t: hsPkgs.${t.passthru.identifier.name or ""} or null;

  # Set of package names the user is iterating on — anything in
  # this set is excluded from the shell's pre-built cabal store, so
  # cabal compiles those packages from the working tree.
  # Mirrors v1's `removeSelectedInputs` semantics: a project package
  # is *only* excluded if it's in `selectedPackages`.  With the
  # default `packages = selectProjectPackages`, every project pkg
  # is excluded (matches "the cabal-project I'm developing").  With
  # an explicit `packages = ps: [ ps.foo ]`, only `foo` is excluded
  # — sibling project pkgs that `foo` depends on get pre-built into
  # the store like any external dep.
  selectedPkgNames = builtins.listToAttrs
    (map (p: { name = p.identifier.name or ""; value = true; })
      (lib.filter (p: p ? identifier) selectedPackages));
  pkgIsSelected = p:
    p != null && (p ? identifier)
    && (selectedPkgNames ? ${p.identifier.name});

  # All components of the selected packages' unit-id fragments
  # plus their setup configs (Custom-build packages' setup-depends).
  # v2 plan-nix puts each cabal unit-id in its own `hsPkgs` entry —
  # `selectedPackages` already enumerates every fragment, so a
  # straight `concatMap getAllComponents` covers every component
  # without an extra closure walk.
  selectedAllComps = selectedComps ++ selectedSetupConfigs;

  # External dep predicate: keep deps whose resolved package isn't
  # in `selectedPkgNames`.  This *includes* sibling project pkgs
  # the user didn't select (so they get pre-built), and SRPs (same
  # story).
  isExternal = d:
    let dp = depPkg d; in
    (d ? identifier)
    && (dp == null || !(pkgIsSelected dp));

  # `haskellLib.dependToLib` resolves a `component.depends` entry
  # to its library derivation: for a top-level lib dep that's
  # `p.components.library`, but for a sublib dep the entry is
  # already the sublib derivation and gets returned unchanged.
  # Filtering out selected packages here is what gives the shell
  # its "selected pkgs aren't pre-installed in the cabal store"
  # property — the user iterates on them, so cabal compiles them
  # from the working tree on demand.
  ownLibDepSlices = lib.filter (s: s != null)
    (map haskellLib.dependToLib
      (lib.filter isExternal
        (lib.concatMap (c: c.config.depends or c.depends or [])
          selectedAllComps)));
  # Same exclusion for build-tool exes carried via
  # `passthru.executableToolDepends` — keep prebuilt external tools
  # (alex, happy, hsc2hs, ...) so cabal sees them in the store, but
  # drop tools whose source is a selected package so cabal rebuilds
  # them from the user's tree.  Also filter to v2 slices (have
  # `passthru.transitiveTarballs`) to skip nixpkgs-side tools like
  # `gcc` or `pkgconf` which aren't haskell-nix-built.
  ownBuildToolSlices = lib.filter
    (t: t != null
        && t ? passthru
        && (t.passthru ? transitiveTarballs)
        && !(pkgIsSelected (exePkg t)))
    (lib.concatMap (c: c.executableToolDepends or []) selectedAllComps);
  ownDepSlices = ownLibDepSlices ++ ownBuildToolSlices;

  # When the user combines this shell with other v2 shells via
  # `inputsFrom` (notably the cross shells the haskell-nix overlay
  # adds when `crossPlatforms` is set), each of those carries its
  # own `passthru.depSlices`.  Without merging them in, the main
  # shell's `composedStore` only contains native slices and
  # `haskell-nix-cabal-store-sync` only seeds those into
  # `~/.cabal/store` — the cross slices that a downstream
  # `${prefix}cabal build` needs would be missing.
  inputsFromDepSlices = lib.concatMap
    (d: (d.passthru or {}).depSlices or [])
    inputsFrom;
  depSlices = haskellLib.uniqueWithName (ownDepSlices ++ inputsFromDepSlices);

  # Composed dep store for the shell.  The same `composeStore`
  # helper that the per-component `.store` attribute uses, applied
  # to the union of every dep slice across the user's selected
  # packages.  Sized linearly with the number of distinct units
  # (lndir → tree of symlinks) rather than the bytes those units
  # take on disk.
  composedStore = composeStore {
    name = "shell-store";
    slices = depSlices;
  };

  # `lndir` reproduces a directory tree as a tree of symlinks —
  # used for installing unit dirs (which are subtrees of files) so
  # the user's cabal store ends up with symlinks pointing into
  # /nix/store rather than full copies.
  lndir = pkgs.buildPackages.lndir or pkgs.buildPackages.xorg.lndir;

  # Standalone sync script placed on PATH as
  # `haskell-nix-cabal-store-sync`.  The shell hook invokes it
  # without arguments; the user can re-run it with `--force` to
  # overwrite conflicting entries.  Its behaviour:
  #   1. Register a nix GC root for the composed-store /nix/store
  #      path so the symlinks we install survive nix-collect-garbage.
  #   2. Scan: compare every conf / unit-dir / lib file in the
  #      composed store against the target cabal store; classify
  #      each as new, same, or conflict.  A previously-synced
  #      symlink whose target has been GC'd counts as new.
  #   3. Bail out if any conflicts exist and `--force` was not
  #      given.
  #   4. Print a cabal-style summary of what will be added.
  #   5. Install missing items (and conflicting items under
  #      `--force`) via `ln -s` for files and `lndir` for unit
  #      directory trees.
  #   6. Recache the merged package.db only when something was
  #      actually installed.
  cabalStoreSync = pkgs.pkgsBuildBuild.writeShellScriptBin "haskell-nix-cabal-store-sync" ''
    set -eu

    force=0
    case "''${1:-}" in
      --force) force=1 ;;
      "") ;;
      *) echo "usage: haskell-nix-cabal-store-sync [--force]" >&2; exit 2 ;;
    esac

    src="${composedStore}"
    tgt_base="''${CABAL_DIR:-$HOME/.cabal}/store"
    ghc_pkg="${ghc}/bin/${ghc.targetPrefix or ""}ghc-pkg"
    lndir="${lndir}/bin/lndir"

    mkdir -p "$tgt_base"

    # ---- Fast path: same composedStore, already synced -----------
    # After a successful run we drop a marker file at
    # `~/.cabal/store/.haskell-nix-shell-markers/<basename-of-src>`.
    # If that marker is present (and `--force` not set), the cabal
    # store is already in sync with this shell's slices and we can
    # skip the per-file scan entirely.  Different shells (different
    # `composedStore`) have distinct basenames, so alternating
    # shells doesn't invalidate each other's markers.
    markers_dir="$tgt_base/.haskell-nix-shell-markers"
    marker="$markers_dir/$(basename "$src")"
    if [ "$force" != "1" ] && [ -f "$marker" ]; then
      exit 0
    fi

    # ---- Register nix GC root ------------------------------------
    # Without a gcroot, nix-collect-garbage would happily delete
    # `$src` (and the slice paths it references), leaving every
    # symlink we install below pointing at nothing.  Each unique
    # composedStore (different deps → different nix hash) gets its
    # own gcroot file; stale gcroots from earlier shells just stay
    # under `.haskell-nix-gcroots/` until the user removes them.
    if command -v nix-store >/dev/null 2>&1; then
      gcroot_dir="$tgt_base/.haskell-nix-gcroots"
      mkdir -p "$gcroot_dir"
      gcroot_link="$gcroot_dir/$(basename "$src")"
      if [ ! -L "$gcroot_link" ] || [ "$(readlink "$gcroot_link")" != "$src" ]; then
        nix-store --realise "$src" --add-root "$gcroot_link" --indirect >/dev/null
      fi
    fi

    # `[ -e PATH ]` follows symlinks, so it's false both when the
    # path doesn't exist and when it's a symlink whose target has
    # been GC'd — exactly the cases we want to treat as "missing"
    # so the install pass below replaces them.

    # Arrays: lines of the form "<kind>\t<ghcDir>\t<relPath>".
    # kind is one of: conf, unit, lib.
    new_items=()
    conflicts=()

    # ---- Pass 1: scan --------------------------------------------
    for ghcDir in "$src"/ghc-*/; do
      [ -d "$ghcDir" ] || continue
      ghcName=$(basename "$ghcDir")
      tgt_ghcDir="$tgt_base/$ghcName"

      # Symlinked single-file entries (conf, lib): if the target is
      # already a symlink pointing at the source path, no work to
      # do — short-circuit before the per-file `diff -q` fork.
      if [ -d "$ghcDir/package.db" ]; then
        for conf in "$ghcDir/package.db"/*.conf; do
          [ -e "$conf" ] || continue
          base=$(basename "$conf")
          tgt="$tgt_ghcDir/package.db/$base"
          if [ ! -e "$tgt" ]; then
            new_items+=("conf	$ghcName	package.db/$base")
          elif [ -L "$tgt" ] && [ "$(readlink "$tgt")" = "$conf" ]; then
            : # already linked correctly
          elif ! diff -q "$conf" "$tgt" >/dev/null 2>&1; then
            conflicts+=("conf	$ghcName	package.db/$base")
          fi
        done
      fi

      # Unit dirs: lndir-installed trees of symlinks.  We can't
      # readlink the dir itself (it's a real dir), so verify a
      # representative leaf — pick the first symlink under the unit
      # tree and check it points back into this `$src/$ghcName/$unitId`.
      # If any leaf does, the whole tree was lndir'd from the same
      # source, so no further work is needed.  Falls through to the
      # full `diff -qr` only when the cheap check fails.
      for unitDir in "$ghcDir"/*/; do
        [ -d "$unitDir" ] || continue
        unitId=$(basename "$unitDir")
        case "$unitId" in lib|package.db|incoming) continue ;; esac
        tgt="$tgt_ghcDir/$unitId"
        if [ ! -e "$tgt" ]; then
          new_items+=("unit	$ghcName	$unitId")
        else
          want="$ghcDir$unitId"
          want="''${want%/}"
          probe=$(find "$tgt" -type l -print -quit 2>/dev/null)
          if [ -n "$probe" ]; then
            target=$(readlink "$probe")
            case "$target" in
              "$want"/*) continue ;;
            esac
          fi
          if ! diff -qr "$unitDir" "$tgt" >/dev/null 2>&1; then
            conflicts+=("unit	$ghcName	$unitId")
          fi
        fi
      done

      if [ -d "$ghcDir/lib" ]; then
        for libFile in "$ghcDir/lib"/*; do
          [ -e "$libFile" ] || continue
          base=$(basename "$libFile")
          tgt="$tgt_ghcDir/lib/$base"
          if [ ! -e "$tgt" ]; then
            new_items+=("lib	$ghcName	lib/$base")
          elif [ -L "$tgt" ] && [ "$(readlink "$tgt")" = "$libFile" ]; then
            : # already linked correctly
          elif ! diff -q "$libFile" "$tgt" >/dev/null 2>&1; then
            conflicts+=("lib	$ghcName	lib/$base")
          fi
        done
      fi
    done

    # ---- Bail on conflicts (unless --force) ----------------------
    if [ "''${#conflicts[@]}" -gt 0 ] && [ "$force" != "1" ]; then
      echo "" >&2
      echo "haskell.nix v2 shell: cabal store has entries that differ from" >&2
      echo "this shell's slices; refusing to overwrite:" >&2
      for c in "''${conflicts[@]}"; do
        IFS=$'\t' read -r kind ghcName rel <<<"$c"
        echo "  $ghcName/$rel" >&2
      done
      echo "" >&2
      echo "To replace these with the shell's versions, run:" >&2
      echo "  haskell-nix-cabal-store-sync --force" >&2
      echo "" >&2
      exit 1
    fi

    # ---- Nothing to do? ------------------------------------------
    if [ "''${#new_items[@]}" -eq 0 ] && [ "''${#conflicts[@]}" -eq 0 ]; then
      exit 0
    fi

    # ---- Summary (cabal-style) -----------------------------------
    echo ""
    if [ "''${#new_items[@]}" -gt 0 ]; then
      echo "The following are being linked into your cabal store:"
      for p in "''${new_items[@]}"; do
        IFS=$'\t' read -r kind ghcName rel <<<"$p"
        echo " - $ghcName/$rel"
      done
    fi
    if [ "''${#conflicts[@]}" -gt 0 ] && [ "$force" = "1" ]; then
      echo ""
      echo "The following are being overwritten (--force):"
      for c in "''${conflicts[@]}"; do
        IFS=$'\t' read -r kind ghcName rel <<<"$c"
        echo " - $ghcName/$rel"
      done
    fi
    echo ""

    # ---- Pass 2: install -----------------------------------------
    # Track which ghcNames actually saw a change — only those need
    # a package.db recache afterwards.
    declare -A touched_ghc

    install_item () {
      # $1 kind, $2 ghcName, $3 rel
      local kind=$1 ghcName=$2 rel=$3
      local src_path="$src/$ghcName/$rel"
      local tgt_path="$tgt_base/$ghcName/$rel"
      mkdir -p "$(dirname "$tgt_path")"
      case "$kind" in
        conf|lib)
          # Single file — replace any existing entry with a symlink
          # to the source.
          if [ -e "$tgt_path" ] || [ -L "$tgt_path" ]; then
            rm -f "$tgt_path"
          fi
          ln -s "$src_path" "$tgt_path"
          ;;
        unit)
          # Directory tree — `lndir` mirrors the source dir as a
          # tree of dirs containing symlinks at the leaves.  Cabal
          # and ghc-pkg follow the symlinks transparently.
          if [ -d "$tgt_path" ] || [ -L "$tgt_path" ]; then
            chmod -R u+w "$tgt_path" 2>/dev/null || true
            rm -rf "$tgt_path"
          fi
          mkdir -p "$tgt_path"
          "$lndir" -silent "$src_path" "$tgt_path"
          ;;
      esac
      touched_ghc[$ghcName]=1
    }

    for p in "''${new_items[@]}"; do
      IFS=$'\t' read -r kind ghcName rel <<<"$p"
      install_item "$kind" "$ghcName" "$rel"
    done
    if [ "$force" = "1" ]; then
      for c in "''${conflicts[@]}"; do
        IFS=$'\t' read -r kind ghcName rel <<<"$c"
        install_item "$kind" "$ghcName" "$rel"
      done
    fi

    # ---- Pass 3: recache (only ghcNames we touched) --------------
    # We install confs via symlinks above; ghc-pkg needs a real
    # writable `package.cache` describing the merged set
    # (composed-store confs plus anything else the user already
    # had in their cabal store) — so regenerate it here.
    for ghcName in "''${!touched_ghc[@]}"; do
      db="$tgt_base/$ghcName/package.db"
      [ -d "$db" ] || continue
      # If a previous run left a `package.cache` symlink (e.g. from
      # a stricter version of this script), it can't be opened for
      # write through /nix/store; drop it so recache can recreate.
      for f in package.cache package.cache.lock; do
        if [ -L "$db/$f" ]; then rm -f "$db/$f"; fi
      done
      "$ghc_pkg" --package-db="$db" recache 2>/dev/null || true
    done

    # ---- Drop sync marker for this composedStore -----------------
    # Lets the next invocation skip the per-file scan via the
    # fast path at the top of this script.
    mkdir -p "$markers_dir"
    : >"$marker"
  '';

  v2ShellHook = ''
    haskell-nix-cabal-store-sync || return 1
  '';

  # Write the project's `cabalProjectLocal` (after all module
  # mkIfs have merged) to a file on shell startup.  For native
  # targets the file is `cabal.project.local`, which cabal picks
  # up automatically.  For cross targets it's
  # `cabal.project.<targetPrefix>local` (the cross `cabal` doesn't
  # auto-discover that path; tests can pull it in with
  # `import: cabal.project.<targetPrefix>local` from their own
  # `cabal.project.local`).
  #
  # If the target file is missing, write it.  If it exists and
  # differs, show the diff and print a command the user can run
  # to force replacement.  Skipped entirely when the project has
  # no `cabalProjectLocal` content.
  cabalProjectLocalContent =
    lib.optionalString (cabalProjectLocal != null && cabalProjectLocal != "") cabalProjectLocal;
  cabalProjectLocalFile =
    pkgs.pkgsBuildBuild.writeText "cabal.project.local" cabalProjectLocalContent;
  cabalProjectLocalSync = pkgs.pkgsBuildBuild.writeShellScriptBin "haskell-nix-cabal-project-local-sync" ''
    set -eu

    force=0
    case "''${1:-}" in
      --force) force=1 ;;
      "") ;;
      *) echo "usage: haskell-nix-cabal-project-local-sync [--force]" >&2; exit 2 ;;
    esac

    target=cabal.project.${ghc.targetPrefix or ""}local
    source=${cabalProjectLocalFile}

    install_it () {
      install -m u+rw,go+r "$source" "$target"
    }

    if [ ! -e "$target" ]; then
      install_it
      echo "Wrote $target (haskell.nix defaults)"
    elif diff -q "$target" "$source" > /dev/null 2>&1; then
      : # already in sync
    elif [ "$force" = "1" ]; then
      install_it
      echo "Overwrote $target with haskell.nix defaults (--force)"
    else
      echo
      echo "Existing $target differs from haskell.nix's defaults:"
      echo
      diff -u "$target" "$source" || true
      echo
      echo "To replace it with the haskell.nix defaults, run:"
      echo "  haskell-nix-cabal-project-local-sync --force"
      echo
    fi
  '';
  cabalProjectLocalShellHook = ''
    haskell-nix-cabal-project-local-sync || true
  '';

  # For cross-compilation shells, expose a `${prefix}cabal` wrapper
  # that auto-passes the right `--with-compiler=` etc. so the user
  # can run plain `${prefix}cabal build` without remembering the
  # cross-GHC paths.  Mirrors v1's wrapper at `shell-for.nix:183`.
  # The wrapper points at a tiny symlink farm (`ghcShim`) that
  # exposes the cross GHC's binaries under both the prefixed name
  # (so cabal's UnitId-hash sees the same `--with-compiler=` path
  # as the slices) and the unprefixed name (so cabal v2-build's
  # "near compiler" lookup finds plain `ghc-pkg`).
  targetPrefix = ghc.targetPrefix or "";
  # Two-pass: see `build-cabal-slice.nix`'s `ghcShim` for the
  # rationale.  GHC 9.14.1's cross GHCs ship both `<prefix>foo`
  # AND `foo` in their bin/ (e.g. `deriveConstants`); the previous
  # single-pass loop crashed when iteration hit the prefixed name
  # first and synthesised an alias before the real unprefixed
  # iteration ran.
  ghcShim = pkgs.pkgsBuildBuild.runCommand "${ghc.name}-shim" {
    preferLocalBuild = true;
  } ''
    mkdir -p $out/bin
    for f in ${ghc}/bin/*; do
      base=$(basename "$f")
      ln -s "$f" "$out/bin/$base"
    done
    ${lib.optionalString (targetPrefix != "") ''
      for f in ${ghc}/bin/${targetPrefix}*; do
        base=$(basename "$f")
        unprefixed=''${base#${targetPrefix}}
        [ -e "$out/bin/$unprefixed" ] || ln -s "$f" "$out/bin/$unprefixed"
      done
    ''}
  '';
  crossCabalWrapper = lib.optional (targetPrefix != "")
    (pkgs.pkgsBuildBuild.writeShellScriptBin "${targetPrefix}cabal" ''
      exec cabal \
        --with-compiler=${ghcShim}/bin/${targetPrefix}ghc \
        --with-hsc2hs=${ghcShim}/bin/${targetPrefix}hsc2hs \
        $(builtin type -P "${targetPrefix}pkg-config" &> /dev/null \
          && echo "--with-pkg-config=${targetPrefix}pkg-config") \
        "$@"
    '');

  # Build tools from hackage via haskell-nix.tool.  Use the project's
  # compiler so the tools are compatible with the shell's GHC.
  compilerNixName = compiler.nix-name;
  toolDrvs = lib.mapAttrsToList
    (name: versionOrMod:
      pkgs.pkgsBuildBuild.haskell-nix.tool compilerNixName name versionOrMod)
    tools;

  # withHoogle: ensure hoogle is on PATH.  If the user already listed
  # it in `tools` use theirs, otherwise pick a sensible default.
  hoogleDrvs = lib.optional
    (withHoogle && !(tools ? hoogle))
    (pkgs.pkgsBuildBuild.haskell-nix.tool compilerNixName "hoogle" {});

  # Collect build-tool-depends for the shell — these are the
  # executables cabal would otherwise have to find or build when it
  # hits a `build-tool-depends` stanza.  v2 components expose them
  # via `.passthru.executableToolDepends` (comp-v2-builder mirrors
  # v1's shape).
  buildToolComponents =
    if allToolDeps
      then lib.concatMap haskellLib.getAllComponents
             (lib.filter (x: !(x.isRedirect or false)) (builtins.attrValues hsPkgs))
      else selectedComps;
  # Drop tools whose source is a selected package — those are
  # things the user is iterating on (e.g. cardano-wallet's own exes
  # that other selected packages list as build-tool-depends).
  # Mirrors `ownBuildToolSlices` and v1's `removeSelectedInputs`.
  # Without this, any selected exe that's a build-tool of another
  # selected package gets pre-built and pulled into the shell
  # closure.
  buildToolDrvs = haskellLib.uniqueWithName
    (lib.filter (t: t != null && !(pkgIsSelected (exePkg t)))
      (lib.concatMap (c: c.executableToolDepends or []) buildToolComponents));

  # Merge inputs from any `inputsFrom` shells (typical nix pattern).
  inputsFromBuildInputs       = lib.concatMap (d: d.buildInputs       or []) inputsFrom;
  inputsFromNativeBuildInputs = lib.concatMap (d: d.nativeBuildInputs or []) inputsFrom;
  inputsFromShellHook         = lib.concatMapStrings (d: (d.shellHook or "") + "\n") inputsFrom;

  # v1's `shellFor` exposes a wrapped ghc at `env.ghc` with a
  # package-db containing the shell's library deps, so callers can
  # use `env.ghc/bin/runghc` or `env.ghc/bin/ghc-pkg list` without
  # needing cabal.  v2 composes its deps into the cabal store
  # instead, so the plain ghc sees none of them.  Mirror v1's API
  # by surfacing the composed store via a GHC package-environment
  # file — lists each composed unit as `package-id <id>` so GHC
  # exposes not just the main library of each dep but also its
  # public sublibs (which GHC hides by default when you only stack
  # the package-db via `GHC_PACKAGE_PATH`).
  # cabal's per-compiler store dir is `ghc-<version>-<abi>` (or just
  # `ghc-<version>` when abi is empty).  GHC ≥ 9.8 reports
  # `Project Unit Id: ghc-<version>-inplace`, from which cabal infers
  # `compiler-abi: inplace`; earlier GHCs omit the field, so cabal
  # records an empty abi and the dir name has no `-inplace` suffix.
  # Mirror that here, otherwise the composedStore lookup misses the
  # actual dir (`/shell-store/ghc-9.6.7/package.db` exists but we'd
  # look at `/shell-store/ghc-9.6.7-inplace/package.db`).
  ghcDirName = "ghc-${ghc.version}"
    + lib.optionalString (builtins.compareVersions ghc.version "9.8" >= 0) "-inplace";
  composedPkgDb = "${composedStore}/${ghcDirName}/package.db";
  globalPkgDb = "${ghc}/lib/ghc-${ghc.version}/lib/package.conf.d";
  # Use `pkgsBuildBuild.runCommand` (build-build stdenv) rather
  # than `pkgs.runCommand` (cross stdenv).  `packageEnv` and
  # `wrappedGhc` end up in the shell's `nativeBuildInputs`; if
  # they're constructed against the cross-host stdenv, mkShell's
  # nativeBuildInputs splicing eventually evaluates the cross
  # stdenv's `cc.isClang` (via setup-hook chains) and throws "no C
  # compiler provided for this platform" on platforms like ghcjs.
  # The shell is a build-platform tool — its helpers belong on the
  # build platform too.
  packageEnv = pkgs.pkgsBuildBuild.runCommand "${ghc.name}-v2-shell-env-file"
    { preferLocalBuild = true; } ''
      mkdir -p $out
      {
        echo "clear-package-db"
        echo "global-package-db"
        echo "package-db ${composedPkgDb}"
        # A package-env file explicitly lists which packages are
        # visible — listed dbs alone aren't enough to expose them,
        # unlike plain `-package-db`.  Walk both the boot db
        # (base, directory, ghc-prim, ...) and the composed store
        # and emit `package-id <id>` for every unit.  Cabal pretty-
        # prints long `id:` values onto the next line indented;
        # short boot ids fit on the same line — handle both.
        for conf in ${globalPkgDb}/*.conf ${composedPkgDb}/*.conf; do
          [ -e "$conf" ] || continue
          awk '
            /^id:/ {
              if (NF >= 2) { print "package-id " $2; exit }
              getline; print "package-id " $1; exit
            }
          ' "$conf"
        done
      } > $out/env
    '';
  # Shared base shim — same one a slice's `cabal v2-build` uses
  # via `--with-compiler=`, so the shell's ghc behaves like the
  # slice's ghc (cabal near-compiler aliases, ghcjs settings
  # patch, native-musl iserv aliases).  No ghcjs ar wrapping is
  # needed in the shell (no cabal v2-build link step here).
  baseShim = makeGhcShim { inherit ghc; };

  # `ghc-pkg` exposure mode: layer makeWrapper env vars on top of
  # the shim so `ghc -e ...` / `ghc-pkg list` see the composed
  # store.  Wrapping a wrapper is fine — the outer one exec's
  # `baseShim/bin/...` which in turn exec's real ghc with `-B`.
  wrappedGhc = pkgs.pkgsBuildBuild.runCommand "${ghc.name}-v2-shell-env"
    { nativeBuildInputs = [ pkgs.pkgsBuildBuild.makeWrapper ];
      preferLocalBuild = true;
      passthru = {
        inherit (ghc) version meta;
        targetPrefix = ghc.targetPrefix or "";
      };
    } ''
      mkdir -p $out/bin
      prefix="${ghc.targetPrefix or ""}"
      # Symlink every shim binary so PATH-driven invocations
      # (alex, happy, hsc2hs, unlit, ...) still find them.
      for f in ${baseShim}/bin/*; do
        ln -s "$f" $out/bin/$(basename "$f")
      done
      # ghc / ghci / runghc / runhaskell / haddock honour
      # GHC_ENVIRONMENT and will pick up our package-id list.
      for prg in ghc ghci ghc-${ghc.version} ghci-${ghc.version} \
                 runghc runhaskell \
                 haddock; do
        if [ -e ${baseShim}/bin/$prefix$prg ]; then
          rm -f $out/bin/$prefix$prg
          makeWrapper \
            ${baseShim}/bin/$prefix$prg \
            $out/bin/$prefix$prg \
            --set GHC_ENVIRONMENT "${packageEnv}/env"
        fi
      done
      # ghc-pkg doesn't read GHC_ENVIRONMENT; use GHC_PACKAGE_PATH
      # so `ghc-pkg list` etc. show the composed store's units.
      for prg in ghc-pkg ghc-pkg-${ghc.version}; do
        if [ -e ${baseShim}/bin/$prefix$prg ]; then
          rm -f $out/bin/$prefix$prg
          makeWrapper \
            ${baseShim}/bin/$prefix$prg \
            $out/bin/$prefix$prg \
            --set GHC_PACKAGE_PATH "${composedPkgDb}:"
        fi
      done
    '';

  # Pick exactly one path for surfacing the dep closure.  In
  # `cabal-store` mode the shell ghc IS the same shim a slice's
  # `cabal v2-build` uses — same `-B<libdir>` flag, same cabal
  # near-compiler aliases, same musl iserv aliases.  In `ghc-pkg`
  # mode we layer env-var wrappers on top to make `ghc-pkg list`
  # and `ghc -e ...` see the composed store directly.
  shellGhc = if exposePackagesVia == "ghc-pkg" then wrappedGhc else baseShim;
  shellSyncTools = lib.optional (exposePackagesVia == "cabal-store") cabalStoreSync
    ++ [ cabalProjectLocalSync ];
  shellSyncHook = lib.optionalString (exposePackagesVia == "cabal-store") v2ShellHook;

in
mkShell {
  nativeBuildInputs =
       [ shellGhc
         # Also include the raw `ghc` so its
         # `depsTargetTargetPropagated` chain reaches the shell's
         # build env — slices already get this via their own
         # `nativeBuildInputs = [ ghc ... ]`; the shim alone (a
         # `runCommand` drv) drops the propagation.  Concretely:
         # wasm32-wasi-ghc propagates `libffi-wasm.out` so the wasm
         # cc-wrapper adds `-L<libffi-wasm>/lib` to
         # `NIX_LDFLAGS_FOR_TARGET`, letting `wasm-ld -lffi` resolve
         # when `cabal v2-build` links a wasm exe inside the shell.
         ghc
       ]
    ++ shellSyncTools
    ++ crossCabalWrapper
    ++ toolDrvs
    ++ hoogleDrvs
    ++ buildToolDrvs
    ++ additionalTools
    # On Darwin the apple-sdk overlay sets `DEVELOPER_DIR` to the
    # nix-store SDK path (just headers/libs, no tools), so the
    # `/usr/bin/git` xcrun shim fails with `tool 'git' not found`.
    # Nix-managed git on PATH bypasses the shim entirely.  Cheap to
    # include unconditionally — most v2 shell users want git on
    # PATH anyway, and `gitMinimal` keeps the closure small.
    ++ [ pkgs.buildPackages.gitMinimal ]
    # `pkg-config` on PATH unconditionally.  cabal's solver probes
    # `pkg-config <name>` whenever a package declares
    # `pkgconfig-depends:` (e.g. `zlib +pkg-config` in this project);
    # if it's missing cabal silently flips `+pkg-config` to
    # `-pkg-config`, and the resulting unit-id diverges from
    # plan-nix's (which was solved with `cabalPkgConfigWrapper`
    # available) — cabal then rebuilds the lib from source instead
    # of reusing the slice in the cabal store.  v1's shell got
    # pkg-config in through each slice's pkgconfig-depends closure
    # via stdenv propagation; v2 slices flow through the cabal store
    # rather than via `buildInputs`, so we add it directly.  Always
    # on, even when no selected pkg has pkgconfig-depends — keeps
    # the unit-id stable if the user adds a pkgconfig dep later, and
    # matches v1's "always present" feel.  `cabalPkgConfigWrapper`
    # is the real pkg-config plus a `--libs --static` failure shim
    # (issue #1642), not the fake `allPkgConfigWrapper` used during
    # plan-nix evaluation.
    ++ [ pkgs.buildPackages.cabalPkgConfigWrapper ]
    ++ nativeBuildInputs
    ++ inputsFromNativeBuildInputs;
  buildInputs = buildInputs
    # Surface every selected component's and dep slice's C-side deps
    # (`component.libs`, `component.pkgconfig`, `component.frameworks`)
    # in the shell.  Each v2 slice exposes these as
    # `passthru.runtimeLibs` (`comp-v2-builder.nix`, the same set
    # that lands in the slice's own `extraBuildInputs`).  Without
    # this, the `pkg-config` we just added to nativeBuildInputs
    # doesn't find any of the project's pkgconfig deps — e.g.
    # `pkg-config --exists zlib` fails because no `zlib.pc` is on
    # `PKG_CONFIG_PATH` — and cabal's solver flips `+pkg-config`
    # off for the affected Haskell packages, diverging the unit-ids
    # from plan-nix.  Mirrors v1's `systemInputs` line in
    # `shell-for.nix:108`, which collects the same set via
    # `c.buildInputs ++ c.propagatedBuildInputs` propagation.
    ++ lib.concatMap (s: s.passthru.runtimeLibs or [])
         (selectedAllComps ++ depSlices)
    ++ inputsFromBuildInputs;
  shellHook =
    cabalProjectLocalShellHook + "\n"
    + shellSyncHook + "\n"
    + inputsFromShellHook
    + shellHook;
  # Expose the composed dep store via passthru (and not as a
  # top-level attr) — recent nixpkgs's `mkDerivation` folds
  # unknown top-level attrs into `env` and type-checks each value,
  # which under cross targets like ghcjs forces evaluation of
  # the host stdenv's `bashInteractive` while checking
  # `isDerivation`, hitting `stdenv.cc.isClang` against a stdenv
  # with no cc and throwing "no C compiler provided for this
  # platform".
  passthru = {
    # Mirror the `.store` attr a v2 component exposes so callers can
    # build the composed dep store standalone with
    # `nix-build -A <project>.shell.store`.
    store = composedStore;
    inherit composedStore depSlices cabalStoreSync cabalProjectLocalSync crossCabalWrapper;
    ghc = shellGhc;
  };
}
