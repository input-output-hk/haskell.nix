{ lib, testSrc, haskell-nix, evalPackages, buildPackages, compiler-nix-name, recurseIntoAttrs }:
let
  project = buildPackages.haskell-nix.project' {
    inherit compiler-nix-name;
    src = evalPackages.fetchgit {
      url = "https://github.com/haskell/haskell-language-server.git";
      fetchSubmodules = true;
      rev = "0.9.0";
      sha256 = "18g0d7zac9xwywmp57dcrjnvms70f2mawviswskix78cv0iv4sk5";
    };
    projectFileName = "stack-${buildPackages.haskell-nix.compiler.${
      if compiler-nix-name == "ghc8104"
        then "ghc8103"
        else compiler-nix-name
    }.version}.yaml";
    sha256map = {
      "https://github.com/alanz/ghc-exactprint.git"."6748e24da18a6cea985d20cc3e1e7920cb743795" = "18r41290xnlizgdwkvz16s7v8k2znc7h215sb1snw6ga8lbv60rb";
      "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
    };
  };
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) stack-nix;
  };
  build = project.hsPkgs.haskell-language-server.components.exes.haskell-language-server;
}
