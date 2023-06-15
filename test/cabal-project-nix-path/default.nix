{ lib, cabalProject', tool, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:
let
  project = cabalProject' {
    name = "cabal-project-nix-path";
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-project-nix-path";
    cabalProject = ''
      packages: ${(tool compiler-nix-name "hello" { inherit evalPackages; }).project.args.src}
    '';
  };
  # The same but with source in a subdir of the store path
  projectSubDir = project.appendModule {
    cabalProject = lib.mkForce ''
      packages: ${evalPackages.runCommand "hello-src" {} "mkdir -p $out && cp -r ${(tool compiler-nix-name "hello" { inherit evalPackages; }).project.args.src} $out/subdir"}/subdir
    '';
  };

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = project.hsPkgs.hello.components.exes.hello;
  buildSubDir = projectSubDir.hsPkgs.hello.components.exes.hello;
}
