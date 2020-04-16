{ stackProject, testSrc }:

let
  project = stackProject {
    src = testSrc "stack-local-resolver";
  };
in

project.stack-local-resolver
