{ callPackage }:

let
  project = callPackage ./project.nix {};
in
project.getComponent "make-install-plan:exe:make-install-plan"
