{ project', recurseIntoAttrs, testSrc }:

let
  project = project' {
    src = testSrc "stack-remote-resolver";
    resolverSha256 = "1rldkqqsxd8zxybrkqhc25bcxinhz212kz45jcz8jinfihc91jl7";
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-remote-resolver.components) library;
}
