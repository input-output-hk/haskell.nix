{ haskellNixSrc ? builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/61b1c8a06c74a83c0d2dc7d937d8daa6b32b2a2f.tar.gz";
      sha256 = "1vi8is7h85sb8acymjcnkjm39fp5pal2wq9p7zdv5cmillzs2sza";
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

