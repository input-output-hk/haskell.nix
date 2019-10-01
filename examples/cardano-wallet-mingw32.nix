with import ../. { nixpkgs = ../../nixpkgs; nixpkgsArgs = { crossSystem = { config = "x86_64-pc-mingw32"; }; }; };
let Cabal = buildPackages.haskell-nix.hackage-package {
    name = "Cabal"; version = "2.4.1.0";
    modules = [
        { packages.Cabal.patches = [ ./Cabal-install-folder.diff ]; }
    ];
}; in
(haskell-nix.stackProject {
    src = ../../cardano-wallet;
    pkg-def-extras = [(hackage: {
      packages = {
      #     "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
      #     "process" = (((hackage.process)."1.6.5.0").revisions).default;
          Win32 = hackage.Win32."2.8.3.0".revisions.default;
      };
    })
    (hackage: {
      packages = {
          "hsc2hs" = (((hackage.hsc2hs)."0.68.4").revisions).default;
        };
      })];
    modules = [
      {
        nonReinstallablePkgs = [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
                                  "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
                                  "ghc-boot" "binary" "bytestring" "filepath"
                                  "directory" "containers"
                                  "time" "unix" "Win32" ];
      }
      { doHaddock = false; }
      {
        # packages.http-client.patches        = pkgs.stdenv.lib.optionals pkgs.stdenv.hostPlatform.isWindows [ ({ version, revision }: if version == "0.5.14" then ../overlays/patches/http-client-0.5.14.patch else null) ];
      }
    ];}).cardano-wallet.components.all
