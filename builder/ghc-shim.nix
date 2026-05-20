# Build a ghc "shim" directory that mirrors `${ghc}/bin/*` but
# applies the small set of tweaks v2 needs from ghc:
#
#   * Cross GHCs ship only `<prefix>ghc-pkg`; cabal v2-build's
#     "near-compiler" lookup expects `ghc-pkg` in the same dir.
#     Add unprefixed aliases for every prefixed binary so cabal
#     finds `ghc-pkg`, `hsc2hs`, `runghc`, etc.
#
#   * ghcjs: lndir real ghc's libdir into the shim, materialise a
#     writable `settings` file, sed-swap the `ar command` entry to
#     `ghcjsArWrapper` (drops missing `.o`s — backpack-on-ghcjs
#     workaround), and wrap `ghc` / `ghc-<v>` with `-B<libdir>` so
#     ghc uses the patched settings.
#
#   * native-musl ghc ≥ 9.10: lndir libdir into the shim, wrap ghc
#     binaries with `-B<libdir>`, and add unprefixed aliases for
#     `unlit` and `ghc-iserv[-dyn|-prof]`.  Mirrors v1's
#     `ghc-for-component-wrapper.nix:136-140`.
#
# Used by:
#   * `build-cabal-slice.nix` — slice's `--with-compiler=` target.
#   * `shell-for-v2.nix` — base of the user-facing shell's ghc;
#     the `ghc-pkg` exposure mode layers env-var wrappers on top.
{ stdenv, lib, pkgsBuildBuild, haskellLib }:

{ ghc
, # if non-null AND ghcjs, sed the `ar command` entry of the
  # patched settings file to point at this wrapper.  Pass `null`
  # to skip the patch (still does the lib lndir, settings copy,
  # and `-B` wrap — useful when the caller doesn't have an ar
  # wrapper handy).
  ghcjsArWrapper ? null
, # extra paths to prefix into `LD_LIBRARY_PATH` when the wrapped
  # ghc binaries run.  Used on native-musl to give iserv-dyn (which
  # ghc spawns for TH eval) the musl-gcc libs dir — its transitive
  # `libstdc++.so` → `libgcc_s.so.1` lookup would otherwise miss.
  # Scoping the env to the ghc wrapper (rather than the whole
  # derivation) keeps glibc subprocesses cabal spawns directly —
  # notably `git` for `source-repository-package` — from loading
  # musl libs and crashing.
  extraLibraryPaths ? []
}:

let
  targetPrefix = ghc.targetPrefix or "";
  ghcBin = "${targetPrefix}ghc";
  nativeMuslNeedsAlias =
    haskellLib.isNativeMusl
    && builtins.compareVersions ghc.version "9.10" >= 0;
  needsLibShim = stdenv.hostPlatform.isGhcjs || nativeMuslNeedsAlias;
in

pkgsBuildBuild.runCommand "${ghc.name}-shim" {
  preferLocalBuild = true;
  nativeBuildInputs = lib.optionals needsLibShim [
    (pkgsBuildBuild.lndir or pkgsBuildBuild.xorg.lndir)
    pkgsBuildBuild.makeWrapper
  ];
  passthru = {
    inherit (ghc) version meta;
    inherit targetPrefix;
  };
} (
  (if stdenv.hostPlatform.isGhcjs then ''
    mkdir -p $out/bin
    ghcLib=$(${ghc}/bin/${ghcBin} --print-libdir)
    libRel=''${ghcLib#${ghc}/}
    mkdir -p "$out/$libRel"
    lndir -silent "$ghcLib" "$out/$libRel"
    settingsFile="$out/$libRel/settings"
    if [ -L "$settingsFile" ]; then
      cp --remove-destination "$(readlink -f "$settingsFile")" "$settingsFile"
    fi
    ${lib.optionalString (ghcjsArWrapper != null) ''
      sed -i 's|("ar command", "[^"]*")|("ar command", "${ghcjsArWrapper}")|' "$settingsFile"
    ''}
    for f in ${ghc}/bin/*; do
      base=$(basename "$f")
      case "$base" in
        ${ghcBin}|${ghcBin}-${ghc.version})
          makeWrapper "$f" "$out/bin/$base" --add-flags "-B$out/$libRel"
          ;;
        *)
          ln -s "$f" "$out/bin/$base"
          ;;
      esac
    done
  ''
  else if nativeMuslNeedsAlias then ''
    mkdir -p $out/bin
    ghcLib=$(${ghc}/bin/${ghcBin} --print-libdir)
    libRel=''${ghcLib#${ghc}/}
    mkdir -p "$out/$libRel"
    lndir -silent "$ghcLib" "$out/$libRel"
    for f in ${ghc}/bin/*; do
      base=$(basename "$f")
      case "$base" in
        ${ghcBin}|${ghcBin}-${ghc.version})
          makeWrapper "$f" "$out/bin/$base" --add-flags "-B$out/$libRel" ${
            lib.concatMapStringsSep " "
              (p: "--prefix LD_LIBRARY_PATH : ${p}")
              extraLibraryPaths
          }
          ;;
        *)
          ln -s "$f" "$out/bin/$base"
          ;;
      esac
    done
    ${
      # Literate pre-processor + iserv lookups.  Guarded on
      # existence so a ghc that lacks `<prefix>ghc-iserv-dyn`
      # (etc.) just skips that one alias instead of dangling.
      lib.concatMapStrings (a: ''
        if [ -e "${ghc}/bin/${targetPrefix}${a}" ] && [ ! -e "$out/bin/${a}" ]; then
          ln -s "${targetPrefix}${a}" "$out/bin/${a}"
        fi
      '') [ "unlit" "ghc-iserv" "ghc-iserv-dyn" "ghc-iserv-prof" ]
    }
  '' else ''
    mkdir -p $out/bin
    for f in ${ghc}/bin/*; do
      base=$(basename "$f")
      ln -s "$f" "$out/bin/$base"
    done
  '')
  + lib.optionalString (targetPrefix != "") ''
    for f in ${ghc}/bin/${targetPrefix}*; do
      base=$(basename "$f")
      unprefixed=''${base#${targetPrefix}}
      [ -e "$out/bin/$unprefixed" ] || ln -s "$base" "$out/bin/$unprefixed"
    done
  ''
)
