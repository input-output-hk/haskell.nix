# Map overrides of the form `packages.${pkg-name}` to all the matching
# packages in the plan.
{pkgs, config, options, ...}: {
  use-package-keys = true;
  package-keys = map (p: p.pkg-name) config.plan-json.install-plan ++ map (p: p.id) config.plan-json.install-plan;
  # A canonical pkg-name can appear in the install plan under
  # several ids (haskell.nix's per-instance UnitIDs — multiple
  # instances that differ only in their dep UnitIDs, or a multi-
  # component package split across per-component entries).  Group
  # the install-plan ids by their pkg-name so consumers can look
  # each up at `config.packages.${id}` instead of guessing by name.
  package-ids-by-name = pkgs.lib.foldl'
    (acc: p: acc // { ${p.pkg-name} = (acc.${p.pkg-name} or []) ++ [ p.id ]; })
    {}
    config.plan-json.install-plan;
  # The install plan keyed by plan id — a name → entry index over
  # `plan-json.install-plan` for O(log N) lookup of a specific
  # plan entry by its haskell.nix per-instance UnitID instead of
  # the linear scan a `lib.findFirst` over the list would do.
  plan-json-by-id = pkgs.lib.listToAttrs
    (map (p: { name = p.id; value = p; }) config.plan-json.install-plan);
  # Project-global v2-builder data derived once from the plan (see
  # `builder/v2-project-globals.nix`).  Lazy: only forced by the v2
  # builder, so v1 / non-plan projects never pay for it.
  plan-json-v2-globals = import ../../builder/v2-project-globals.nix {
    inherit (pkgs) lib;
  } {
    planJson = config.plan-json.install-plan;
    isWasm = pkgs.stdenv.hostPlatform.isWasm;
    ghcVersion = config.compiler.version;
  };
  packages = pkgs.lib.listToAttrs (map (p: {
      name = p.id;
      value = pkgs.lib.modules.mkAliasDefinitions (options.packages.${p.pkg-name});
    }) (pkgs.lib.filter (p: p.id != p.pkg-name) config.plan-json.install-plan));
}
