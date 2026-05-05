{pkgs, lib, config, ...}: {
  nonReinstallablePkgs = ["rts" "base" "ghc-prim" "integer-gmp" "integer-simple"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "8.11" >= 0) [
      "ghc-bignum"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "9.9" >= 0) [
      "ghc-internal"]
    # The v1 builder hides ghci-related packages on ghcjs/wasm because
    # those builds rely on the GHC-bundled versions; reinstalling them
    # via the standard component path tends to clash with what GHC
    # itself shipped.  v2 follows cabal's install plan literally — if
    # cabal decided to reinstall e.g. `filepath-1.5.5.0`, we just
    # build a slice for it like any other dep.  So skip these
    # entries when `builderVersion = 2`.
    ++ lib.optionals
         ((pkgs.stdenv.hostPlatform.isGhcjs || pkgs.stdenv.hostPlatform.isWasm)
           && config.builderVersion != 2)
         ([
      # ghci and its dependencies
      "ghci" "binary" "bytestring" "containers" "template-haskell" "array" "deepseq" "file-io" "filepath" "ghc-boot" "ghc-boot-th" "ghc-heap" "transformers" "unix" "directory" "time" "ghc-platform" "os-string" "exceptions" "stm" "ghc-experimental"]
    ++ lib.optionals (builtins.compareVersions config.compiler.version "8.11" < 0) [
      "ghcjs-prim" "ghcjs-th"]);
}
