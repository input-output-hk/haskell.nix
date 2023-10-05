{ stackProject', recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

let
  project = stackProject' {
    src = testSrc "stack-compiler";
    inherit evalPackages;
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  meta.disabled = compiler-nix-name != "ghc947";
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-compiler.components) library;
}
