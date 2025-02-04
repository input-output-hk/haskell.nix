{ stdenv, lib, pkgs, mkStackPkgSet, haskellLib, testSrc, compiler-nix-name, evalPackages }:

with lib;

let
  project = pkgs.haskell-nix.stackProject' {
    src = testSrc "stack-simple";
    inherit evalPackages;
  };

  packages = project.hsPkgs;

in pkgs.recurseIntoAttrs {
  meta.disabled = compiler-nix-name != "ghc984";
  stack-simple-exe = (haskellLib.check packages.stack-simple.components.exes.stack-simple-exe) // {
      # Attributes used for debugging with nix repl
      inherit pkgSet packages;
  };
  stack-simple-test = packages.stack-simple.checks.stack-simple-test;
  stack-simple-checks = packages.stack-simple.checks;
  stack-simple-shell = packages.shellFor { tools = { cabal = "3.6.2.0"; }; };
}
