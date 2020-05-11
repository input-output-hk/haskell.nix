{ haskellNixSrc ? builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/3178c84e162dadc6e740eef3d56391d3a0f1c228.tar.gz";
      sha256 = "0bwgibp8vaavnqxzw6iphhzrcr7mr59fgm9pa05csbx1paxzirim";
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

