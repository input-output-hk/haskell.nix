# Test a package set
{ stdenv, lib, util, cabalProject', haskellLib, testSrc, compiler-nix-name, evalPackages, evalSystem, testCabalProjectLocal, testInputMap }:

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
    inherit compiler-nix-name evalSystem;
    src = testSrc "cabal-simple-prof";
    inputMap = testInputMap;
    cabalProjectLocal = testCabalProjectLocal
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
        -- text-iso8601 (reached via aeson) is the one library in this
        -- closure whose profiled compile does not fit in memory:
        -- `Data.Time.FromText` is SIGKILLed under
        -- `-O -prof -fprof-auto-exported` on BOTH x86_64-linux and
        -- aarch64-darwin, while the vanilla way compiles the same two
        -- modules in about two seconds.  It is the only failing step in the
        -- job, which has never passed on ghc914-sh (it passes on ghc9103
        -- and ghc9141).
        --
        -- Turn off the automatic cost-centre annotation, NOT the profiling
        -- way: cabal-simple's exe is profiled and links
        -- cabal-simple -> aeson -> text-iso8601, so dropping the `.p_o`
        -- objects (`library-profiling: False`) would trade the OOM for a
        -- link failure.  `none` maps to `mempty` in Cabal's
        -- `profDetailLevelFlag`, so `-prof` is still passed and only
        -- `-fprof-auto-exported` -- the part that blows up -- goes away.
        -- text-iso8601 then carries no cost centres of its own, which is
        -- fine here: the test profiles cabal-simple, not its dependencies.
        --
        -- It must be spelled `profiling-detail`, which sets the library
        -- level too.  `library-profiling-detail: none` looks more precise
        -- but does the wrong thing -- plan.json then records
        -- `--profiling-detail=none --library-profiling-detail=default`, and
        -- Cabal's library level is `profDetail <> profLibDetail`
        -- (Configure.hs `tryLibProfileLevel`), so the `default` on the right
        -- wins and the library is still built with `-fprof-auto-exported`.
        -- Verified via plan.json's configure-args, which is what the v2
        -- builder reads (see the note above).
        --
        -- The underlying blowup is NOT diagnosed: ghc914-sh differs from the
        -- passing compilers in GHC point release (9.14.0 vs 9.14.1), builder
        -- (v2 slices vs v1 Setup.hs) and build configuration all at once,
        -- and isolating them needs a compiler build.  If that is ever chased
        -- and fixed, this stanza should go.
        package text-iso8601
          profiling-detail: none
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
