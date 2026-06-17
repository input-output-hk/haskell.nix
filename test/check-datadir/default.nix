# A package with a library that ships `data-files`, and a test-suite that
# reads them via `getDataFileName`.  Running the test's `check` exercises that
# the builder makes the package data-dir available to tests — in particular
# under builderVersion = 2, where the check runs the installed binary directly
# and `lib/check.nix` must set `<pkg>_datadir`.
{ lib, stdenv, project', testSrc, compiler-nix-name, evalPackages }:

let
  mkProject = builderVersion: project' {
    inherit compiler-nix-name evalPackages builderVersion;
    src = testSrc "check-datadir";
  };

  project = mkProject 1;
  projectV2 = mkProject 2;

in lib.recurseIntoAttrs ({
  ifdInputs = {
    inherit (project) plan-nix;
    plan-nix-v2 = projectV2.plan-nix;
  };

  run = project.hsPkgs.check-datadir.checks.test;

  # The test spawns a build-tool-depends exe (readProcess) and reads its
  # data-files / a source-relative file — this can't be reproduced when the test
  # binary runs under an emulator (Windows/wine, Android), which can't reliably
  # spawn the build-tool.  Disable the whole test there.
  meta.disabled = stdenv.hostPlatform.isWindows || stdenv.hostPlatform.isAndroid;
}
# The v2 check stages data-files as absolute /nix/store symlinks and points
# Cabal at them via the `<pkg>_datadir` env var, but wasmtime neither forwards
# host env vars to the wasm guest nor follows absolute symlinks (no CLI option
# for either), so this check can't run on wasm.  (The v1 `run` works on wasm:
# its data-files are real files in a `-data` output.)  See overlays/wasm.nix.
// lib.optionalAttrs (!stdenv.hostPlatform.isWasm) {
  run-v2 = projectV2.hsPkgs.check-datadir.checks.test;
})
