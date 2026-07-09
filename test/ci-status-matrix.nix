# Guard test for the auto-generated "Compiler CI status" table in README.md.
#
# scripts/update-ci-status.sh enumerates a hand-maintained COMPILERS array to
# decide which GHCs to query on Hydra.  If a GHC is added to (or removed from)
# the CI matrix in ci.nix without updating that array, the README table would
# silently omit (or keep advertising) a compiler.  This test fails the build in
# that case, so the two are kept in sync.
#
# `ghcs` is the set of GHC compilers actually present in hydraJobs (the keys of
# the "GHC version" dimension); `scriptFile` is scripts/update-ci-status.sh.
{ lib, pkgs, ghcs, scriptFile }:

let
  # Extract the COMPILERS=(...) array from the script.  It must be a single
  # line (the script has a comment saying so), but we tolerate leading
  # indentation and any trailing content such as an inline comment.  The
  # capture stops at the first ')', so trailing text after it is ignored.
  scriptText = builtins.readFile scriptFile;
  matchLine = l:
    let m = builtins.match "[[:space:]]*COMPILERS=\\(([^)]*)\\).*" l;
    in if m == null then null else builtins.head m;
  compilersInner = lib.findFirst (m: m != null) null
    (map matchLine (lib.splitString "\n" scriptText));
  scriptCompilers = if compilersInner == null then null
    else builtins.filter (s: s != "")
      (lib.splitString " " (builtins.replaceStrings [ "\t" ] [ " " ] compilersInner));

  hydraGhcs = lib.sort (a: b: a < b) (lib.unique ghcs);

  # GHCs in hydraJobs but missing from the script, and vice versa.
  missing = if scriptCompilers == null then [] else lib.subtractLists scriptCompilers hydraGhcs;
  stale = if scriptCompilers == null then [] else lib.subtractLists hydraGhcs scriptCompilers;

  fail = msg: ''
    echo "FAIL: ${msg}" >&2
    exit 1
  '';
in
pkgs.runCommand "ci-status-matrix" { } (
  if scriptCompilers == null then
    fail "could not find a single-line 'COMPILERS=(...)' array in scripts/update-ci-status.sh"
  else if hydraGhcs == [] then
    fail "could not determine any GHCs from hydraJobs (test wiring is broken)"
  else if missing == [] && stale == [] then ''
    echo "OK: scripts/update-ci-status.sh COMPILERS matches the GHCs in hydraJobs:" >&2
    echo "  ${toString hydraGhcs}" >&2
    touch $out
  '' else ''
    echo "FAIL: scripts/update-ci-status.sh COMPILERS is out of sync with hydraJobs." >&2
    echo "  GHCs in hydraJobs:       ${toString hydraGhcs}" >&2
    echo "  COMPILERS in the script: ${toString scriptCompilers}" >&2
    ${lib.optionalString (missing != [])
       ''echo "  -> ADD to COMPILERS: ${toString missing}" >&2''}
    ${lib.optionalString (stale != [])
       ''echo "  -> REMOVE from COMPILERS: ${toString stale}" >&2''}
    echo "Update COMPILERS (and PLATFORMS if a new target/pin is involved) in" >&2
    echo "scripts/update-ci-status.sh, then re-run it to refresh README.md." >&2
    exit 1
  ''
)
