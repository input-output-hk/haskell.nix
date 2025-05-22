{ lib, cabalProject', tool, recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:
let
  # Kind of round about way of getting the source for the hello package from hackage
  # so we can use it in this test.
  hello-src = evalPackages.runCommand "hello-src" { nativeBuildInputs = [ evalPackages.gnutar ]; } ''
    mkdir -p $out
    tar -xzf ${(tool compiler-nix-name "hello" { inherit evalPackages; }).src} -C $out
    mv $out/hello-*/* $out
  '';
  project = cabalProject' {
    name = "cabal-project-nix-path";
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-project-nix-path";
    cabalProject = ''
      packages: ${hello-src}
    '';
  };
  # The same but with source in a subdir of the store path
  projectSubDir = project.appendModule {
    cabalProject = lib.mkForce ''
      packages: ${evalPackages.runCommand "hello-src-in-subdir" {} "mkdir -p $out && cp -r ${hello-src} $out/subdir"}/subdir
    '';
  };

in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  build = project.hsPkgs.hello.components.exes.hello;
  buildSubDir = projectSubDir.hsPkgs.hello.components.exes.hello;
}
