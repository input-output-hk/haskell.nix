{ stackProject', recurseIntoAttrs, testSrc }:

let
  project = stackProject' {
    src = testSrc "stack-local-resolver";
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-local-resolver.components) library;
}
