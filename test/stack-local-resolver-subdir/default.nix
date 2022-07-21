{ project', recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

let
  project = project' {
    src = testSrc "stack-local-resolver-subdir";
    inherit evalPackages;
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  meta.disabled = compiler-nix-name != "ghc865";
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-local-resolver.components) library;
}
