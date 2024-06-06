# Test that embedFile function for the file-embed package works
{ stdenv, lib, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "embed-file";
    cabalProjectLocal = builtins.readFile ../cabal.project.local
      + lib.optionalString (haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64) ''
        constraints: text -simdutf
    '';
  };

  packages = project.hsPkgs;

in recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs
    # Could not load 'filezmembedzm0zi0zi16zi0zmL8bqDH6rAX64X4nLQOoPcy_DataziFileEmbed_makeRelativeToProject_closure', dependency unresolved. See top entry above.
    || (builtins.elem compiler-nix-name ["ghc928"] && !haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64 && stdenv.hostPlatform.isMusl)
    # /build/source-test-embed-file-root-test-embed-file-exe-embed-file-root/test/embed-file/app: getDirectoryContents:openDirStream: invalid argument (Invalid argument)
    || (builtins.elem compiler-nix-name ["ghc928"] && haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64 && stdenv.hostPlatform.isMusl)
    # Failed to lookup symbol: __aarch64_swp8_acq_rel
    || (builtins.elem compiler-nix-name ["ghc947" "ghc948"] && haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64)
    ;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.embed-file.components.exes.embed-file;
  check = haskellLib.check build;
  # build-profiled = packages.embed-file.components.exes.embed-file.profiled;
  # check-profiled = haskellLib.check build-profiled;
}
