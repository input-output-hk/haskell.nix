# Mark everything in the install-plan as "planned".
# Uses plain `true` instead of `mkOverride 900 true` because the
# plan is definitive -- no other module sets `planned` on these
# components, so the override priority machinery is unnecessary.
#
# Extracts component keys directly from the plan.json structure.
# The plan-json component prefixes (e.g. `exe:foo`) are inverted from
# `haskellLib.componentPrefix` so that adding a new component type to
# that single source of truth flows through here automatically.
{componentPrefix}:
{config, lib, ...}:
let
  # Invert componentPrefix (collection -> prefix) into prefix -> collection,
  # e.g. { exes = "exe"; tests = "test"; ... } becomes
  #      { exe = "exes"; test = "tests"; ... }
  prefixMap = lib.listToAttrs
    (lib.mapAttrsToList (collection: prefix: { name = prefix; value = collection; })
      componentPrefix);
  # Extract the module-system component structure from a plan entry
  # without invoking dependency resolution.
  plannedComponents = p:
    let
      rawComps = p.components or { ${p.component-name or "lib"} = {}; };
    in
      # library and setup are top-level keys (not nested)
      lib.optionalAttrs (rawComps ? lib) { library = { planned = true; }; }
      // lib.optionalAttrs (rawComps ? setup) { setup = { planned = true; }; }
      # Prefixed components (exe:foo, test:bar, etc.) become nested: exes.foo, tests.bar
      // lib.foldl' (acc: n:
        let
          parts = builtins.match "([^:]+):(.*)" n;
          prefix = builtins.elemAt parts 0;
          name = builtins.elemAt parts 1;
          collection = prefixMap.${prefix} or null;
        in if parts == null || collection == null then acc
           else acc // { ${collection} = (acc.${collection} or {}) // { ${name} = { planned = true; }; }; }
      ) {} (builtins.attrNames rawComps);
in {
  packages = lib.listToAttrs (map (p: {
    name = p.id;
    value.components = plannedComponents p;
  }) config.plan-json.install-plan);
}
