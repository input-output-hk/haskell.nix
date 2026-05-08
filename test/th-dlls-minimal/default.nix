# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  # See `docs/dev/profiling.md` — v2 expects profiling toggles in
  # cabal.project so plan-nix records `--enable-library-profiling`.
  project = { externalInterpreter, profiled ? false }: project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "th-dlls-minimal";
    cabalProjectLocal = builtins.readFile ../cabal.project.local
      + lib.optionalString profiled ''
        package *
          library-profiling: True
      '';
    modules = [
     ({pkgs, ...}: {
      packages.th-dlls-minimal.components.library.preBuild = ''
        export ISERV_ARGS=-v
        export PROSY_ARGS=-v
      '';
      packages.test-lib.components.library.libs = mkForce [
        (pkgs.stdenv.mkDerivation {
          name = "test-clib";
          version = "1.0";
          src = testSrc "th-dlls-minimal/test-clib";
        })
      ];
     })
     ({pkgs, ...}: lib.optionalAttrs externalInterpreter {
      packages.th-dlls-minimal.ghcOptions = [ "-fexternal-interpreter" ];
    })];
  };

  packages         = (project { externalInterpreter = false;                  }).hsPkgs;
  packages-ei      = (project { externalInterpreter = true;                   }).hsPkgs;
  packages-prof    = (project { externalInterpreter = false; profiled = true; }).hsPkgs;
  packages-ei-prof = (project { externalInterpreter = true;  profiled = true; }).hsPkgs;

in lib.recurseIntoAttrs {
  # This test is just for windows currently (the full th-dlls test runs on other platforms)
  meta.disabled = !stdenv.hostPlatform.isWindows
    # Rosetta error: invalid gdt selector index 5 (wine crashes under Rosetta with msvcrt)
    || (stdenv.hostPlatform.isWindows && stdenv.hostPlatform.libc != "ucrt")
    ;

  ifdInputs = {
    plan-nix             = (project { externalInterpreter = false;                  }).plan-nix;
    plan-nix-ei          = (project { externalInterpreter = true;                   }).plan-nix;
    plan-nix-profiled    = (project { externalInterpreter = false; profiled = true; }).plan-nix;
    plan-nix-profiled-ei = (project { externalInterpreter = true;  profiled = true; }).plan-nix;
  };

  build                    = packages.th-dlls-minimal.components.library;
  build-profiled           = packages-prof.th-dlls-minimal.components.library;
  just-template-haskell    = packages.th-dlls-minimal.components.exes.just-template-haskell;
  build-ei                 = packages-ei.th-dlls-minimal.components.library;
  build-profiled-ei        = packages-ei-prof.th-dlls-minimal.components.library;
  just-template-haskell-ei = packages-ei.th-dlls-minimal.components.exes.just-template-haskell;
}
