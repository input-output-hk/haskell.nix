{ testSrc, evalPackages, buildPackages, compiler-nix-name, recurseIntoAttrs }:
let
  project = buildPackages.haskell-nix.project' {
    inherit compiler-nix-name;
    src = evalPackages.fetchgit {
      url = "https://github.com/haskell/haskell-language-server.git";
      fetchSubmodules = true;
      rev = "e390da54a90f2c6c02ce836a27f2d4c33a71f607";
      sha256 = "0s77r79zn6q42k3biylni81vhn1yr8r1mqn8pj9kgggh16xw229b";
    };
    projectFileName = "cabal.project";
    sha256map = {
      "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
    };
    cabalProjectLocal = ''
      allow-newer: diagrams-svg:base, monoid-extras:base, svg-builder:base,
        diagrams-lib:base, dual-tree:base, active:base, diagrams-core:base,
        diagrams-contrib:base, force-layout:base, diagrams-postscript:base,
        statestack:base
    '';
  };
in recurseIntoAttrs {
  ifdInputs = {
    inherit (project) plan-nix;
  };
  build = project.hsPkgs.haskell-language-server.components.exes.haskell-language-server;
}
