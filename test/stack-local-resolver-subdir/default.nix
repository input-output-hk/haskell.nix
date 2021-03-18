{ project', recurseIntoAttrs, testSrc }:

let
  project = project' {
    src = testSrc "stack-local-resolver-subdir";
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-local-resolver.components) library;
}
