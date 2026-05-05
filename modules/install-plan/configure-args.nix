# Extract program options from plan.json configure-args and apply them
# to the corresponding packages.  This allows options set in a
# cabal.project file (via `package` or `program-options` stanzas)
# to be picked up automatically by haskell.nix.
#
# --ghc-option / --ghcjs-option  →  ghcOptions  (dedicated option)
# --configure-option=            →  configureOptions (cabal-project round-trip)
# other --*-option=              →  configureFlags  (passed through)
{config, lib, ...}: {
  packages = lib.listToAttrs (lib.concatMap (p:
    let
      configureArgs = p.configure-args or [];
      # The first element is the command name ("configure"), skip it.
      args = if configureArgs != [] then builtins.tail configureArgs else [];

      # Extract --ghc-option=VALUE and --ghcjs-option=VALUE entries
      # into the dedicated ghcOptions option.
      #
      # Filter `-hide-all-packages`: cabal injects it on every Setup
      # configure call (it explicitly hides all packages and re-adds
      # them via --dependency=).  Round-tripping it through the v2
      # builder's `package <n>\n  ghc-options:` block would cause a
      # duplicate `--ghc-option=-hide-all-packages` to appear in the
      # slice's `pkgHashProgramArgs` while a downstream cabal-build
      # only sees it once — forking the UnitId hash and defeating
      # cache reuse.
      ghcOptions = lib.concatMap (arg:
        let
          m = builtins.match "--(ghc|ghcjs)-option=(.*)" arg;
          opt = if m != null then builtins.elemAt m 1 else null;
        in lib.optional (m != null && (config.builderVersion == 1 || opt != "-hide-all-packages")) opt
      ) args;

      # Extract --configure-option=VALUE entries into the dedicated
      # configureOptions option.  These come from cabal.project's
      # `configure-options:` stanza and the v2 builder emits them
      # back into its own cabal.project so UnitIds stay consistent
      # between a slice build and downstream consumers.
      configureOptions = lib.concatMap (arg:
        let m = builtins.match "--configure-option=(.*)" arg;
        in lib.optional (m != null) (builtins.elemAt m 0)
      ) args;

      # Extract other --PROG-option=VALUE entries (gcc, ld, hsc2hs,
      # alex, happy, c2hs, cpphs, ghc-pkg, …) and pass them through
      # as configureFlags.  `--ghc-option` and `--configure-option`
      # are handled above.
      otherFlags = builtins.filter (arg:
        let
          isProgOption = builtins.match "--[a-z][a-z0-9-]*-option=.*" arg != null;
          isGhcOption = builtins.match "--(ghc|ghcjs)-option=.*" arg != null;
          isCfgOption = builtins.match "--configure-option=.*" arg != null;
        in isProgOption && !isGhcOption && !isCfgOption
      ) args;

      value =
        lib.optionalAttrs (ghcOptions != []) { inherit ghcOptions; }
        // lib.optionalAttrs (configureOptions != []) { inherit configureOptions; }
        // lib.optionalAttrs (otherFlags != []) { configureFlags = otherFlags; };
    in lib.optional (value != {}) {
      name = p.id;
      inherit value;
    }
  ) config.plan-json.install-plan);
}
