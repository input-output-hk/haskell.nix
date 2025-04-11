# Add `hsPkgs.${pkg-name}` based on the available targets in the plan.
{pkgs, lib, config, ...}:
let
  redirect = existing: redirectName: packageTargets:
    let
      componentsByName = builtins.listToAttrs (map (x: { name = x.component-name; value = x.available; }) packageTargets);
      lookupComponent = collectionName: name: available:
        let attrPath =
          if collectionName == ""
            then "${redirectName}.components.library"
            else "${redirectName}.components.${collectionName}.${name}";
        in if builtins.length available != 1
          then throw "Multiple avaialble targets for ${attrPath}"
        else if builtins.isString (builtins.head available)
          then throw "${builtins.head available} looking for ${attrPath}"
        else if collectionName == ""
          then existing.${(builtins.head available).id}.components.library
        else existing.${(builtins.head available).id}.components.${collectionName}.${name};
      componentsWithPrefix = collectionName: prefix:
        lib.listToAttrs (lib.concatLists (lib.mapAttrsToList (n: available:
          lib.optional (lib.hasPrefix "${prefix}:" n && (builtins.length available != 1 || !builtins.elem (builtins.head available) ["TargetNotBuildable" "TargetNotLocal"])) (
            let
              name = lib.removePrefix "${prefix}:" n;
              value = lookupComponent collectionName name available;
            in { inherit name value; }
          )) componentsByName));
      defaultTargetId = (builtins.head (
          # Use the package identified by the library component
          componentsByName.lib or
          # Or by the first component
          componentsByName.${builtins.head (builtins.attrNames componentsByName)}
        )).id;
      defaultTargetPackage = existing.${defaultTargetId};
    in defaultTargetPackage // rec {
      isRedirect = redirectName != defaultTargetId;
      identifier = rec { name = (builtins.head packageTargets).pkg-name; version = (builtins.head packageTargets).pkg-version; id = "${name}-${version}"; };
      components =
        lib.mapAttrs componentsWithPrefix pkgs.haskell-nix.haskellLib.componentPrefix
        // lib.optionalAttrs (componentsByName ? lib) {
          library = lookupComponent "" "" componentsByName.lib;
        };
      checks = pkgs.recurseIntoAttrs (
        lib.filterAttrs (_: x: x != {}) (
          builtins.mapAttrs
            (_: d: pkgs.haskell-nix.haskellLib.check d)
              (lib.filterAttrs (_: d: d.config.doCheck) components.tests)));
    };
in {
  options.hsPkgs = lib.mkOption {
    type = lib.types.unspecified;
    apply = existing: existing //
      # Redirects with just the package name
      builtins.removeAttrs (builtins.mapAttrs (packageName: packageTargets:
        let
          byVersion = builtins.groupBy (x: x.pkg-version) packageTargets;
          versions = builtins.attrNames byVersion;
        in if builtins.length versions != 1
          then let
              err = throw "Multiple versions for ${packageName} ${builtins.toJSON versions}";
            in {
              isRedirect = true;
              identifier = { name = packageName; version = err; };
              components = err;
              checks = err;
            }
          else redirect existing packageName packageTargets) (builtins.groupBy (x: x.pkg-name) config.plan-json.targets)) config.preExistingPkgs

      # Redirect for `${name}-${version}`
      // builtins.mapAttrs (packageNameAndVersion: packageTargets: redirect existing packageNameAndVersion packageTargets)
            (builtins.groupBy (x: "${x.pkg-name}-${x.pkg-version}") config.plan-json.targets);
  };
}
