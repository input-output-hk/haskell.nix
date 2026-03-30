# Extract program options from plan.json configure-args and apply them
# to the corresponding packages.  This allows options set in a
# cabal.project file (via `package` or `program-options` stanzas)
# to be picked up automatically by haskell.nix.
#
# --ghc-option / --ghcjs-option  →  ghcOptions  (dedicated option)
# other --*-option=              →  configureFlags  (passed through)
{config, lib, ...}: {
  packages = lib.listToAttrs (lib.concatMap (p:
    let
      configureArgs = p.configure-args or [];
      # The first element is the command name ("configure"), skip it.
      args = if configureArgs != [] then builtins.tail configureArgs else [];

      # Extract --ghc-option=VALUE and --ghcjs-option=VALUE entries
      # into the dedicated ghcOptions option.
      ghcOptions = lib.concatMap (arg:
        let
          m = builtins.match "--(ghc|ghcjs)-option=(.*)" arg;
        in lib.optional (m != null) (builtins.elemAt m 1)
      ) args;

      # Extract other --PROG-option=VALUE entries (gcc, ld, hsc2hs,
      # configure, alex, happy, c2hs, cpphs, ghc-pkg, …) and pass
      # them through as configureFlags.
      otherFlags = builtins.filter (arg:
        let
          isProgOption = builtins.match "--[a-z][a-z0-9-]*-option=.*" arg != null;
          isGhcOption = builtins.match "--(ghc|ghcjs)-option=.*" arg != null;
        in isProgOption && !isGhcOption
      ) args;

      value =
        lib.optionalAttrs (ghcOptions != []) { inherit ghcOptions; }
        // lib.optionalAttrs (otherFlags != []) { configureFlags = otherFlags; };
    in lib.optional (value != {}) {
      name = p.id;
      inherit value;
    }
  ) config.plan-json.install-plan);
}
