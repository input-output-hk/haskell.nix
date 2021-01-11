{ lib, testSrc, haskell-nix, evalPackages, buildPackages, compiler-nix-name, recurseIntoAttrs }:
let
  project = buildPackages.haskell-nix.project' {
    inherit compiler-nix-name;
    src = evalPackages.fetchgit {
      url = "https://github.com/haskell/haskell-language-server.git";
      fetchSubmodules = true;
      rev = "0.7.1";
      sha256 = "0gkzvjx4dgf53yicinqjshlj80gznx5khb62i7g3kqjr85iy0raa";
    };
    projectFileName = "stack-${buildPackages.haskell-nix.compiler.${
      if compiler-nix-name == "ghc8103"
        then "ghc8102"
        else compiler-nix-name
    }.version}.yaml";
    sha256map = {
      "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
    };
  };
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) stack-nix;
  };
  build = project.hsPkgs.haskell-language-server.components.exes.haskell-language-server;
}
