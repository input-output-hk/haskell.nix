{ stackProject', recurseIntoAttrs }:

let
  project = stackProject' {
    src = ./.;
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-source-repo.components) library;
}
