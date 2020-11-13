final: prev:
{
  haskell-nix = prev.haskell-nix // ({
    defaultModules = prev.haskell-nix.defaultModules ++ final.lib.optional final.stdenv.hostPlatform.isGhcjs (
      ({ pkgs, buildModules, config, lib, ... }:
        let
          # Cabal project to build cabal for use in default setups 
          cabal-project = final.buildPackages.haskell-nix.hackage-project {
            name = "cabal-install";
            version = "3.2.0.0";
            compiler-nix-name = config.compiler.nix-name;
            modules = [{
              packages.Cabal.patches = [
                ./patches/Cabal/Cabal-3.0.0.0-drop-pkg-db-check.diff
                ./patches/Cabal/Cabal-3.0.0.0-no-final-checks.diff
              ];
              nonReinstallablePkgs = [ "array" "base" "binary" "bytestring" "containers" "deepseq"
                                       "directory" "filepath" "ghc" "ghc-boot" "ghc-boot-th" "ghc-compact"
                                       "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp"
                                       "libiserv" "mtl" "parsec" "pretty" "process" "rts" "stm"
                                       "template-haskell" "terminfo" "text" "time" "transformers" "unix"
                                       "xhtml"
                                     ];
              }
            ];
          };
        in {
          # Override Cabal used for default setup
          setup-depends = [ cabal-project.hsPkgs.Cabal ];
          # Allow Cabal to be reinstalled so that custom setups will use a Cabal
          # built with packages.Cabal.patches
          nonReinstallablePkgs =
            [ "rts" "ghc-heap" "ghc-prim" "integer-gmp" "integer-simple" "base"
              "deepseq" "array" "ghc-boot-th" "pretty" "template-haskell"
              "ghcjs-prim" "ghcjs-th"
            ]
            ++ lib.optionals (!config.reinstallableLibGhc) [
              "ghc-boot"
              "ghc" "Win32" "array" "binary" "bytestring" "containers"
              "directory" "filepath" "ghc-boot" "ghc-compact" "ghc-prim"
              "hpc"
              "mtl" "parsec" "process" "text" "time" "transformers"
              "unix" "xhtml" "terminfo"
            ];
          # Include patches for custom setups
          packages.Cabal.patches = [
            ./patches/Cabal/Cabal-3.0.0.0-drop-pkg-db-check.diff
            ./patches/Cabal/Cabal-3.0.0.0-no-final-checks.diff
          ];
          testWrapper = [((final.writeScriptBin "node-wrapper" ''
            set -euo pipefail
            exe=$1
            shift
            ${final.buildPackages.nodejs}/bin/node $exe $@
          '') + "/bin/node-wrapper")];
        }
      )
    );
  });
}
