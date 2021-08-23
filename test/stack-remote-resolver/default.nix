{ project', recurseIntoAttrs, testSrc, compiler-nix-name }:

let
  project = project' {
    src = testSrc "stack-remote-resolver";
    resolverSha256 = "1rldkqqsxd8zxybrkqhc25bcxinhz212kz45jcz8jinfihc91jl7";
  };
  packages = project.hsPkgs;

in recurseIntoAttrs {
  meta.disabled = compiler-nix-name != "ghc865";
  ifdInputs = {
    inherit (project) stack-nix;
  };
  inherit (packages.stack-remote-resolver.components) library;
}
