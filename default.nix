{ haskellNixSrc ? builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/f78841c2d05c6c686d3888131104360944464dc2.tar.gz";
      sha256 = "1zwhskx9drd0266kcrxf5vzcgs9vfiiibzkc2h3yarfb2xl7185q";
    }
, nixpkgs ? (import haskellNixSrc {}).sources.nixpkgs-default
, pkgs ? import nixpkgs (import haskellNixSrc {}).nixpkgsArgs
, haskellCompiler ? "ghc883"
}:
let
  project = pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; name = "nix-tools"; };
    ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
    modules = [{
     nonReinstallablePkgs= [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
      "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
      # ghcjs custom packages
      "ghcjs-prim" "ghcjs-th"
      "ghc-boot"
      "ghc" "Win32" "array" "binary" "bytestring" "containers"
      "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
      # "ghci" "haskeline"
      "hpc"
      "mtl" "parsec" "process" "text" "time" "transformers"
      "unix" "xhtml"
      # "stm" "terminfo"
     ];
    }];
  };
in
  project // {
    shell = project.shellFor {
      tools = { cabal = "3.2.0.0"; };
    };
  }

