{ haskellNixSrc ? builtins.fetchTarball {
      url = "https://github.com/input-output-hk/haskell.nix/archive/a84e3b55c642e7aec57c9677158cf446d90e9759.tar.gz";
      sha256 = "0qnchf0f8d5k6363mcrqv936wz6zljs06bjvg1i6xmkg31bdj48m";
    }
, nixpkgs ? haskellNixSrc + "/nixpkgs"
, pkgs ? import nixpkgs (import haskellNixSrc)
, haskellCompiler ? "ghc865"
}:
  pkgs.haskell-nix.cabalProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };
    ghc = pkgs.haskell-nix.compiler.${haskellCompiler};
  }

