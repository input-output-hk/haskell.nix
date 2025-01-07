# Map overrides of the form `packages.${pkg-name}` to all the matching
# packages in the plan.
{pkgs, config, options, ...}: {
  use-package-keys = true;
  package-keys = map (p: p.pkg-name) config.plan-json.install-plan ++ map (p: p.id) config.plan-json.install-plan;
  packages = pkgs.lib.listToAttrs (map (p: {
      name = p.id;
      value = pkgs.lib.modules.mkAliasDefinitions (options.packages.${p.pkg-name});
    }) (pkgs.lib.filter (p: p.id != p.pkg-name) config.plan-json.install-plan));
}
