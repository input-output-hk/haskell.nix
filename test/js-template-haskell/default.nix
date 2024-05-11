# Test building TH code that needs DLLs when cross compiling for windows
{ stdenv, lib, project', haskellLib, recurseIntoAttrs, testSrc, compiler-nix-name, ... }:

with lib;

let
  project = project' {
    inherit compiler-nix-name;
    src = testSrc "js-template-haskell";
    modules = [
      # Fix node: createProcess: posix_spawnp: does not exist (No such file or directory)
      # ({ pkgs,... }: {
      #   packages.js-template-haskell.components.library.build-tools = [ pkgs.pkgsBuildHost.nodejs ];
      # })
    ];
  };

  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = packages.js-template-haskell.components.library;
  build-profiled = packages.js-template-haskell.components.library.profiled;
}
