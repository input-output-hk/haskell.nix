# Extract ghc-options from plan.json configure-args and apply them
# to the corresponding packages.  This allows ghc-options set in
# a cabal.project file (via the `package` or `program-options` stanzas)
# to be picked up automatically by haskell.nix.
{config, lib, ...}: {
  packages = lib.listToAttrs (lib.concatMap (p:
    let
      configureArgs = p.configure-args or [];
      # The first element is the command name ("configure"), skip it.
      args = if configureArgs != [] then builtins.tail configureArgs else [];
      # Extract --ghc-option=VALUE and --ghcjs-option=VALUE entries
      ghcOptions = lib.concatMap (arg:
        let
          m = builtins.match "--(ghc|ghcjs)-option=(.*)" arg;
        in lib.optional (m != null) (builtins.elemAt m 1)
      ) args;
    in lib.optional (ghcOptions != []) {
      name = p.id;
      value.ghcOptions = ghcOptions;
    }
  ) config.plan-json.install-plan);
}
