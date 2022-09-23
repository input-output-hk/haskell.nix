{pkgs, config, ...}@projectArgs: {
  name = "nix-tools";
  cabalProjectLocal = ''
    allow-newer: Cabal:base, cryptohash-sha512:base, haskeline:base
  '';
  materialized = ../../materialized + "/${config.compiler-nix-name}/nix-tools";
  modules = [
    # Work around issue when using older ghc on new MacOS versions.  The
    # old process library used by these versions of ghc uses `fork` in a
    # way that can sometimes while evaluating template haskell.
    ({pkgs, ...}: projectArgs.pkgs.lib.mkIf (pkgs.stdenv.hostPlatform.isDarwin
        && __elem config.compiler-nix-name [ "ghc865" "ghc881" "ghc881" "ghc882" "ghc883" "ghc884" "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc810420210212" "ghc8105" "ghc8106" ]
      ) { packages.hnix.components.library.ghcOptions = [ "-j1" ]; }
    )
    {
      packages.transformers-compat.components.library.doExactConfig = true;
      packages.time-compat.components.library.doExactConfig = true;
      packages.time-locale-compat.components.library.doExactConfig = true;
      # Make Cabal reinstallable
      reinstallableLibGhc = false;
      nonReinstallablePkgs =
        [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
          "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
          "ghc-boot"
          "ghc" "Win32" "array" "binary" "bytestring" "containers"
          "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
          "hpc"
          "mtl" "parsec" "process" "text" "time" "transformers"
          "unix" "xhtml"
        ];
  }];

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
 }
