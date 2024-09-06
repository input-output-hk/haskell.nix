# Mark everthing in the install-plan as "planned"
{getComponents}:
{config, lib, ...}: {
  packages = lib.listToAttrs (map (p:
    let components =
      if p ? component-name
        then { ${p.component-name} = { inherit (p) depends exe-depends; }; }
        else p.components or {};
    in {
        name = p.id;
        value.components = lib.mapAttrs (type: x:
            if type == "library" || type == "setup"
              then { planned = lib.mkOverride 900 true; }
              else
                lib.mapAttrs (_: _: {
                  planned = lib.mkOverride 900 true;
                }) x
          ) (getComponents null {} p);
      }) config.plan-json.install-plan);
}
