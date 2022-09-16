{ compiler-nix-name ? "ghc8107"
}:
let
  sources = import ./nix/sources.nix {};
  haskellNix = import sources."haskell.nix" {};
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
in
  pkgs.haskell-nix.cabalProject {
    inherit compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "nix-tools"; };
    modules = [
      { nonReinstallablePkgs =
          [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
            "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
            "ghc-boot"
            "ghc" "Win32" "array" "binary" "bytestring" "containers"
            "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
            "hpc"
            "mtl" "parsec" "process" "text" "time" "transformers"
            "unix" "xhtml"
          ];
      }
    ];
    shell = {
      tools.cabal = {};
      buildInputs = [
        pkgs.nix-prefetch-git
      ];
    };
  }

