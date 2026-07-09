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
  # Extract the COMPILERS=(...) array from the script.  It is expected to be a
  # single line (the script has a comment saying so for this reason).
  scriptText = builtins.readFile scriptFile;
  compLine = lib.findFirst (lib.hasPrefix "COMPILERS=(") null
    (lib.splitString "\n" scriptText);
  captured = if compLine == null then null
    else builtins.match "COMPILERS=\\((.*)\\)" compLine;
  scriptCompilers = if captured == null then null
    else builtins.filter (s: s != "") (lib.splitString " " (builtins.head captured));

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
