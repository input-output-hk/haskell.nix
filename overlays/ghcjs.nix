final: prev:
{
  haskell-nix = prev.haskell-nix // ({
    defaultModules = prev.haskell-nix.defaultModules ++ final.lib.optional final.stdenv.hostPlatform.isGhcjs ({
        testWrapper = [((final.writeScriptBin "node-wrapper" ''
          set -euo pipefail
          exe=$1
          shift
          ${final.buildPackages.nodejs}/bin/node $exe $@
        '') + "/bin/node-wrapper")];
      }
    );
  });
}
