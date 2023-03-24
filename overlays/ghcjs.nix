final: prev:
{
  haskell-nix = prev.haskell-nix // ({
    defaultModules = prev.haskell-nix.defaultModules ++ final.lib.optional final.stdenv.hostPlatform.isGhcjs (
      ({ pkgs, buildModules, config, lib, ... }: {
        testWrapper = [((final.writeScriptBin "node-wrapper" ''
          set -euo pipefail
          exe=$1
          shift
          ${final.buildPackages.nodejs-18_x}/bin/node $exe $@
        '') + "/bin/node-wrapper")];

        # Apply the patches that came with `ghcjs`
        # Also add a "Keep alive" message to prevent hydra timeouts when hsc2hs runs
        packages = pkgs.lib.genAttrs (pkgs.lib.optionals (__elem config.compiler.nix-name ["ghc865" "ghc884" "ghc8107"]) ["base" "directory" "filepath" "ghc-prim" "integer-gmp" "process" "template-haskell" "time" "unix" "Win32" ])
          (name: {
            components.library.preConfigure = ''
              tr -d '\r' < ${name}.cabal > ${name}.cabal-new
              mv ${name}.cabal-new ${name}.cabal
              patch -p3 < ${pkgs.buildPackages.haskell-nix.compiler.${config.compiler.nix-name}.project.configured-src}/lib/patches/${name}.patch
            '';
            components.library.preBuild = ''
              # Avoid timeouts while unix package runs hsc2hs (it does not print anything
              # for more than 900 seconds).
              KEEP_ALIVE_TMP=$(mktemp -d)
              (
                for n in {1..300}; do
                  if [ ! -f $KEEP_ALIVE_TMP/done ]; then
                    sleep 10
                    if (( $n % 30 == 0 )); then
                      echo "Keep alive: ${name} package build has been running for $(( n / 6 )) minute(s)"
                    fi
                  fi
                done
              ) &
            '';
            components.library.postBuild = ''
              echo ok > $KEEP_ALIVE_TMP/done
            '';
          });
      })
    );
  });
}
