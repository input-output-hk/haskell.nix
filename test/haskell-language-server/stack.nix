{ lib, testSrc, haskell-nix, evalPackages, buildPackages, compiler-nix-name }:

(buildPackages.haskell-nix.project {
  inherit compiler-nix-name;
  src = evalPackages.fetchgit {
    url = "https://github.com/haskell/haskell-language-server.git";
    fetchSubmodules = true;
    rev = "8d37a64f359950f3b340ec50ed419742758ebf0c";
    sha256 = "0w2v133yk0bhnbsj7hnsh5w2kzyd86r6xrkx4q167jyrfq8sa1qg";
  };
  projectFileName = "stack-${haskell-nix.compiler.${if compiler-nix-name == "ghc884" then "ghc883" else compiler-nix-name}.version}.yaml";
  modules = [{ config.compiler.nix-name = lib.mkForce compiler-nix-name; }];
  sha256map = {
    "https://github.com/DanielG/cabal-helper.git"."79a5608778493bf32e74b54bbf1ea2729941e50f" = "1jsiwg94yy8pwhzi3z6ayja9qdgf7fl6xn1h9z681j6lhbx225f8";
  };
}).haskell-language-server.components.exes.haskell-language-server
