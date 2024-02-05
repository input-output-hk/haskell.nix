inputs: pkgs:

let

  apply-hnix-patches = {
    packages.hnix.patches = [
      (builtins.toFile "plutus-core.patch" ''
        diff --git a/src/Nix/Options/Parser.hs b/src/Nix/Options/Parser.hs
        index 3aeb0e5..bea0ac9 100644
        --- a/src/Nix/Options/Parser.hs
        +++ b/src/Nix/Options/Parser.hs
        @@ -214,11 +214,7 @@ versionOpt = shortVersionOpt <*> debugVersionOpt
           debugVersionOpt =
             infoOption
               ( fold
        -          [ "Version: ", showVersion version
        -          , "\nCommit: ", $(gitHash)
        -          , "\n  date: ", $(gitCommitDate)
        -          , "\n  branch: ", $(gitBranch)
        -          ]
        +          [ "Version: ", showVersion version ]
               )
               (  long "long-version"
               <> help "Show long debug version form"

      ''
      )
    ];
  };


  # Fix compilation with newer ghc versions
  apply-workaround-for-ghc94-and-above = { lib, config, ... }:
    lib.mkIf (lib.versionAtLeast config.compiler.version "9.4") {
      # lib:ghc is a bit annoying in that it comes with it's own build-type:Custom, and then tries
      # to call out to all kinds of silly tools that GHC doesn't really provide.
      # For this reason, we try to get away without re-installing lib:ghc for now.
      reinstallableLibGhc = false;
    };


  apply-dontStrip-to-nix-tools = {
    # This is stupid. We should be able to set dontStrip globally.
    # The fact that we can't inherit is bullshit.
    packages.nix-tools.components.exes = {
      cabal-name.dontStrip = false;
      cabal-to-nix.dontStrip = false;
      hackage-to-nix.dontStrip = false;
      hashes-to-nix.dontStrip = false;
      lts-to-nix.dontStrip = false;
      make-install-plan.dontStrip = false;
      plan-to-nix.dontStrip = false;
      stack-repos.dontStrip = false;
    };
  };


  add-static-libs-to-darwin = pkgs.lib.mkIf pkgs.hostPlatform.isDarwin {
    packages.cabal-install.ghcOptions = [
      "-L${pkgs.lib.getLib pkgs.static-gmp}/lib"
    ];
    packages.hpack.ghcOptions = [
      "-L${pkgs.lib.getLib pkgs.static-gmp}/lib"
    ];
    packages.nix-tools.ghcOptions = [
      "-L${pkgs.lib.getLib pkgs.static-gmp}/lib"
      "-L${pkgs.lib.getLib pkgs.static-libsodium-vrf}/lib"
      "-L${pkgs.lib.getLib pkgs.static-secp256k1}/lib"
      "-L${pkgs.lib.getLib pkgs.static-openssl}/lib"
      "-L${pkgs.lib.getLib pkgs.static-libblst}/lib"
    ];
  };


  static-nix-tools-project = pkgs.haskell-nix.project' {

    compiler-nix-name = "ghc928";

    src = ../.;

    inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP; };

    modules = [
      apply-hnix-patches
      apply-workaround-for-ghc94-and-above
      apply-dontStrip-to-nix-tools
      add-static-libs-to-darwin
    ];
  };

in

static-nix-tools-project
