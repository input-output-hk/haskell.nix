{ lib, testSrc, haskell-nix, evalPackages, buildPackages, compiler-nix-name, recurseIntoAttrs }:
let
  project = buildPackages.haskell-nix.project' {
    inherit compiler-nix-name;
    src = evalPackages.fetchgit {
      url = "https://github.com/haskell/haskell-language-server.git";
      fetchSubmodules = true;
      rev = "e390da54a90f2c6c02ce836a27f2d4c33a71f607";
      sha256 = "0s77r79zn6q42k3biylni81vhn1yr8r1mqn8pj9kgggh16xw229b";
    };
    projectFileName = "stack-${haskell-nix.compiler.${compiler-nix-name}.version}.yaml";
    modules = [{ config.compiler.nix-name = lib.mkForce compiler-nix-name; }];
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
