{ stackProject', recurseIntoAttrs, testSrc }:

let
  project = stackProject' {
    src = testSrc "stack-source-repo";
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-source-repo.components) library;
}
