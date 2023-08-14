{ stdenv, project', recurseIntoAttrs, testSrc, compiler-nix-name, evalPackages }:

let
  project = project' {
    inherit compiler-nix-name evalPackages;
    src = testSrc "cabal-project-repo";
    repository = [{
      name = "cardano-haskell-packages";
      src = evalPackages.fetchFromGitHub {
        owner = "input-output-hk";
        repo = "cardano-haskell-packages";
        rev = "5726d16ea0f440f705594bef95adbc6bdbf7778b";
        hash = "sha256-uCEuNvy77nTQ5A1PGj+lvnTx1S7faQ/dXS1RDsBagLY=";
      };
    }];
  };

  inherit (project.hsPkgs) cabal-project-repo;

in
recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };

  test-shell = (project.shellFor {
    tools = { cabal = "latest"; };
  });

  run = stdenv.mkDerivation {
    name = "cabal-project-repo";

    buildCommand = ''
      exe="${cabal-project-repo.components.exes.cabal-project-repo.exePath}"

      size=$(command stat --format '%s' "$exe")
      printf "size of executable $exe is $size. \n" >& 2

      printf "checking whether executable runs... " >& 2
      $exe > $out
    '';

    passthru = {
      inherit project;
    };
  };
}
