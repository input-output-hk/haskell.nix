{ lib, testSrc, haskell-nix, evalPackages, buildPackages, compiler-nix-name, recurseIntoAttrs }:
let
  project = buildPackages.haskell-nix.project' {
    inherit compiler-nix-name;
    src = evalPackages.fetchgit {
      url = "https://github.com/haskell/haskell-language-server.git";
      fetchSubmodules = true;
      rev = "c966e6f8b7be1ec7ca8dc5084fe7f2e6432c50f0";
      sha256 = "1msjprk4g5v7aqpaa8zg34q999yxz0hg7zavc8a89p7yczss9h28";
    };
    projectFileName = "stack-${haskell-nix.compiler.${if compiler-nix-name == "ghc8102" then "ghc8101" else compiler-nix-name}.version}.yaml";
    modules = [{ config.compiler.nix-name = lib.mkForce compiler-nix-name; }];
    sha256map = {
      "https://github.com/DanielG/cabal-helper.git"."79a5608778493bf32e74b54bbf1ea2729941e50f" = "1jsiwg94yy8pwhzi3z6ayja9qdgf7fl6xn1h9z681j6lhbx225f8";
      "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
    };
  };
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) stack-nix;
  };
  build = project.hsPkgs.haskell-language-server.components.exes.haskell-language-server;
}
