# Diff `dummy-ghc --info` (the eval-time stand-in cabal-install
# runs against during plan-to-nix) against the real GHC's `--info`.
# Divergence here translates to plan-nix recording different
# `pkgHashConfigInputs` than the slice's real `cabal v2-build`
# computes, which forks UnitIds and breaks the slice's
# expected-package check on cross targets.
#
# Filters out:
#   * `/nix/store/<hash>-...` paths (LibDir, command paths, etc.)
#     — these legitimately differ since dummy-ghc doesn't actually
#     ship binaries.
#   * Fields the dummy intentionally omits because cabal doesn't
#     read them for elaboration.
#
# Anything else differing is a real misalignment: fix `lib/dummy-ghc.nix`
# until this test passes for the targets that matter.
{ stdenv, lib, pkgs, evalPackages, compiler-nix-name, buildPackages, haskellLib
, testSrc ? null  # `callTest` passes this; we don't need a source dir.
}:

let
  # The real cross / native GHC the project would actually build with.
  realGhc = buildPackages.haskell-nix.compiler.${compiler-nix-name};

  # Construct the dummy-ghc the same way `lib/call-cabal-project-to-nix.nix`
  # does at plan-to-nix time.  Using the same function ensures the
  # test exercises the actual code path.
  dummyGhc = import ../../lib/dummy-ghc.nix {
    inherit pkgs evalPackages;
    ghc = realGhc;
  };

  ghcCmd = "${realGhc.targetPrefix}ghc";

  # Fields cabal-install doesn't consult during elaboration, OR which
  # legitimately differ between dummy and real (paths, internal git
  # commit ids, etc.).  Stripping them lets the diff focus on fields
  # that actually shape `pkgHashConfigInputs`.
  ignoredFields = [
    # Tool / library paths — dummy doesn't ship binaries.
    "C compiler command" "C compiler flags" "C compiler link flags"
    "C compiler supports -no-pie"
    "C++ compiler command" "C++ compiler flags"
    "CPP command" "CPP flags"
    "Haskell CPP command" "Haskell CPP flags"
    "JavaScript CPP command" "JavaScript CPP flags"
    "C-- CPP command" "C-- CPP flags" "C-- CPP supports -g0"
    "ld supports compact unwind" "ld supports filelist" "ld supports single module"
    "ld is GNU ld"
    "Merge objects command" "Merge objects flags" "Merge objects supports response files"
    "ar command" "ar flags" "ar supports at file" "ar supports -L"
    "ranlib command" "otool command" "install_name_tool command"
    "windres command" "unlit command"
    "LLVM target" "LLVM llc command" "LLVM opt command"
    "LLVM llvm-as command" "LLVM llvm-as flags"
    "Use inplace MinGW toolchain"
    "LibDir" "Global Package DB"
    "Project name" "Project Git commit id"
    # Numeric breakdowns (cabal uses Project version directly).
    "Project version" "Project Version Int"
    "Project Patch Level" "Project Patch Level1" "Project Patch Level2"
    "Booter version"
    # Per-arch nitpicks cabal doesn't drive UnitId hashing from.
    "target word size" "target word size in bits" "target word big endian"
    "target os string" "target arch string"
    "target has GNU nonexec stack" "target has .ident directive"
    "target has subsections via symbols" "target has libm"
    "target has RTS linker"
    "Unregisterised"
    "Support SMP" "Support parallel --make"
    "Object splitting supported"
    "Target default backend"
    "Have native code generator"
    "Leading underscore"
    "Use LibFFI"
    "RTS expects libdw"
    "Relative Global Package DB"
    "base unit-id"
    "ghc-internal Unit Id"
    "GHC Profiled" "Debug on"
    # Backpack/keys/IDs — every modern GHC says YES to these.
    "Support Backpack"
    "Requires unified installed package IDs"
    "Uses package keys" "Uses unit IDs"
  ];

  # Strip Nix-store paths (legitimate differences), drop any line
  # mentioning an `ignoredFields` key, drop alist syntax/whitespace
  # noise, and sort so we don'\''t care about field order (cabal
  # uses the alist as a map).  Escape regex metacharacters in field
  # names — most notably `+` in `C++ compiler command`.
  escapeRegex = s: builtins.replaceStrings
    [ "+" "(" ")" "." "*" "?" "[" "]" "{" "}" "|" "$" "^" ]
    [ "\\+" "\\(" "\\)" "\\." "\\*" "\\?" "\\[" "\\]" "\\{" "\\}" "\\|" "\\$" "\\^" ]
    s;
  filterScript = pkgs.buildPackages.writeText "filter-ghc-info.sh" ''
    #!${pkgs.buildPackages.runtimeShell}
    sed -E \
        -e 's|/nix/store/[a-z0-9]+-[^"[:space:]]*|<store-path>|g' \
        -e 's|^[[:space:]]*\[\(|,(|' \
      | grep -vE '"(${
        builtins.concatStringsSep "|" (map escapeRegex ignoredFields)
      })"' \
      | grep -E '^[[:space:]]*,[[:space:]]*\("' \
      | sort
  '';
in stdenv.mkDerivation {
  name = "dummy-ghc-info-test-${realGhc.name}";

  meta = {
    # Diffing makes sense only when both ghcs are buildable on the
    # current host platform — otherwise we'd be building cross-ghcs
    # we can't run.  The test belongs in cross-target jobsets.
    platforms = lib.platforms.all;
  };

  buildCommand = ''
    set -euo pipefail
    real=$(mktemp)
    dummy=$(mktemp)
    real_filtered=$(mktemp)
    dummy_filtered=$(mktemp)

    echo "real ghc:  ${realGhc}/bin/${ghcCmd}" >&2
    echo "dummy ghc: ${dummyGhc}/bin/${ghcCmd}" >&2

    ${realGhc}/bin/${ghcCmd}  --info > "$real"  || { echo "real --info failed"  >&2; exit 1; }
    ${dummyGhc}/bin/${ghcCmd} --info > "$dummy" || { echo "dummy --info failed" >&2; exit 1; }

    # Pretty-print one field per line so diff is readable.
    pretty() { sed -E 's/,\("/\n,("/g' "$1"; }
    pretty "$real"  | sh ${filterScript} > "$real_filtered"
    pretty "$dummy" | sh ${filterScript} > "$dummy_filtered"

    if ! diff -u "$dummy_filtered" "$real_filtered" > /tmp/dummy-vs-real.diff; then
      echo "" >&2
      echo "===== dummy-ghc --info diverges from real ghc --info =====" >&2
      cat /tmp/dummy-vs-real.diff >&2
      echo "" >&2
      echo "Fix \`lib/dummy-ghc.nix\` until the diff is empty (or" >&2
      echo "extend \`ignoredFields\` here if the diverging field is" >&2
      echo "demonstrably not consulted by cabal-install for elaboration)." >&2
      exit 1
    fi

    cp "$dummy_filtered" $out
  '';

  passthru = { inherit dummyGhc realGhc; };
}
