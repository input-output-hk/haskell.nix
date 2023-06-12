{ cabalProject', tool, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:
let
  helloSrc = (tool compiler-nix-name "hello" { inherit evalPackages; }).project.args.src;
  project = cabalProject' {
    name = "cabal-project-nix-path";
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-project-nix-path";
    cabalProject = ''
      packages: ${helloSrc}
    '';
  };

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = project.hsPkgs.hello.components.exes.hello;
  buildWithStorePaths = (project.appendModule { storePaths = [helloSrc]; }).hsPkgs.hello.components.exes.hello;
}
