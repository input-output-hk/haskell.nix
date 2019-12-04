{ stackProject }:

let
  project = stackProject {
    src = ./.;
  };
in

project.stack-local-resolver
