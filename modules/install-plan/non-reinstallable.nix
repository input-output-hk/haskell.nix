{pkgs, lib, config, ...}: {
  nonReinstallablePkgs = ["rts" "base" "ghc-prim" "integer-gmp" "integer-simple"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "8.11" >= 0) [
      "ghc-bignum"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "9.9" >= 0) [
      "ghc-internal"]
    ++ lib.optionals (pkgs.stdenv.hostPlatform.isGhcjs) ([
      # ghci and its dependencies
      "ghci" "binary" "bytestring" "containers" "template-haskell" "array" "deepseq" "filepath" "ghc-boot" "ghc-boot-th" "ghc-heap" "transformers" "unix" "directory" "time" "ghc-platform" "os-string"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "8.11" < 0) [
      "ghcjs-prim" "ghcjs-th"]);
}