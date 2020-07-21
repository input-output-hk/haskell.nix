{ testSrc, evalPackages, buildPackages, compiler-nix-name }:

(buildPackages.haskell-nix.project {
  inherit compiler-nix-name;
  src = evalPackages.fetchgit {
    url = "https://github.com/haskell/haskell-language-server.git";
    fetchSubmodules = true;
    rev = "8d37a64f359950f3b340ec50ed419742758ebf0c";
    sha256 = "0w2v133yk0bhnbsj7hnsh5w2kzyd86r6xrkx4q167jyrfq8sa1qg";
  };
  projectFileName = "cabal.project";
  cabalProjectLocal = ''
    allow-newer: diagrams-svg:base, monoid-extras:base, svg-builder:base,
      diagrams-lib:base, dual-tree:base, active:base, diagrams-core:base,
      diagrams-contrib:base, force-layout:base, diagrams-postscript:base,
      statestack:base
  '';
}).haskell-language-server.components.exes.haskell-language-server
