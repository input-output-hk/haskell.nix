# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, util, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages, buildPackages }:

with lib;

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "exe-dlls";
    cabalProjectLocal = builtins.readFile ../cabal.project.local;
    modules = import ../modules.nix ++ optional stdenv.hostPlatform.isAndroid { 
      packages.libsodium.configureFlags = [ "--c2hs-option=--cppopts=-D_Null_unspecified=" ];
      packages.libsodium.components.library.hardeningDisable = ["fortify"];
    };
  };

  packages = project.hsPkgs;

in recurseIntoAttrs rec {
  meta.disabled = stdenv.hostPlatform.isGhcjs;

  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.exe-dlls.components.exes.exe-dlls;
  check = haskellLib.check build;
  build-profiled = packages.exe-dlls.components.exes.exe-dlls.profiled;
  check-profiled = haskellLib.check build-profiled;
}
