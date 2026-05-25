# Test a package set
{ stdenv, lib, util, cabalProject', haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  modules = [
    {
      # Package has no exposed modules which causes
      #   haddock: No input file(s)
      packages.cabal-simple.doHaddock = false;
      packages.cabal-simple.enableProfiling = true;
      enableLibraryProfiling = true;
      # executableProfiling = false;
    }
  ];

  project = cabalProject' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-simple-prof";
    cabalProjectLocal = builtins.readFile ../cabal.project.local
      + lib.optionalString (haskellLib.isCrossHost && stdenv.hostPlatform.isAarch64) ''
        constraints: text -simdutf, text source
      ''
      # v2 reads profiling settings from plan.json's `configure-args`
      # (the cabal-install-recorded toggles), not from haskell.nix's
      # module-level `enableProfiling`/`enableLibraryProfiling`.  Mirror
      # the modules above in cabal.project shape so plan-nix records
      # the matching configure-args, keeping the slice's UnitId
      # reproducible.  The module-level overrides are kept for v1.
      + ''
        package *
          library-profiling: True
        package cabal-simple
          profiling: True
      '';
    inherit modules;
  };

  exe = (project.getComponent "cabal-simple:exe:cabal-simple")
    .override (lib.optionalAttrs stdenv.hostPlatform.isAndroid { setupBuildFlags = ["--ghc-option=-optl-static" "--ghc-option=-optl-ldl"]; });

in lib.recurseIntoAttrs {
  meta.disabled = stdenv.hostPlatform.isGhcjs || stdenv.hostPlatform.isWasm;
  ifdInputs = {
    inherit (project) plan-nix;
  };
  run = stdenv.mkDerivation {
    name = "cabal-simple-prof-test";

    buildCommand = ''
      exe="${exe.exePath}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      # fixme: run on target platform when cross-compiled
      printf "checking whether executable runs with profiling... " >& 2
      # Curiosity: cross compilers prodcing profiling with `+RTS -p -h` lead to the following cryptic message:
      #   cabal-simple: invalid heap profile option: -h*
      # Hence we pass `-hc`.
      ${toString exe.config.testWrapper} $exe +RTS -p -hc

      touch $out
    '';

    meta = {
      platforms = platforms.all;
    };

    passthru = {
      # Used for debugging with nix repl
      inherit project;
    };
  };
}
