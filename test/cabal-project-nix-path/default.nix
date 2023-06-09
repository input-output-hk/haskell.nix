{ cabalProject', tool, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:
let
  project = cabalProject' {
    name = "cabal-project-nix-path";
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-project-nix-path";
    cabalProject = ''
      packages: ${(tool compiler-nix-name "hello" { inherit evalPackages; }).project.args.src}
    '';
  };

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = project.hsPkgs.hello.components.exes.hello;
}
