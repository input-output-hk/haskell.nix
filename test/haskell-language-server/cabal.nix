{ testSrc, evalPackages, buildPackages, compiler-nix-name }:

(buildPackages.haskell-nix.project {
  inherit compiler-nix-name;
  src = evalPackages.fetchgit {
    url = "https://github.com/haskell/haskell-language-server.git";
    fetchSubmodules = true;
    rev = "c966e6f8b7be1ec7ca8dc5084fe7f2e6432c50f0";
    sha256 = "1msjprk4g5v7aqpaa8zg34q999yxz0hg7zavc8a89p7yczss9h28";
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
}).haskell-language-server.components.exes.haskell-language-server
