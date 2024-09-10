# Add `hsPkgs.${pkg-name}` based on the available targets in the plan.
{pkgs, lib, config, ...}:
let
  redirect = redirectName: packageTargets:
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
          then config.hsPkgs.${(builtins.head available).id}.components.library
        else config.hsPkgs.${(builtins.head available).id}.components.${collectionName}.${name};
      componentsWithPrefix = collectionName: prefix:
        lib.listToAttrs (lib.concatLists (lib.mapAttrsToList (n: available:
          lib.optional (lib.hasPrefix "${prefix}:" n && (builtins.length available != 1 || !builtins.elem (builtins.head available) ["TargetNotBuildable" "TargetNotLocal"])) (
            let
              name = lib.removePrefix "${prefix}:" n;
              value = lookupComponent collectionName name available;
            in { inherit name value; }
          )) componentsByName));
    in rec {
      isRedirect = true;
      identifier = rec { name = (builtins.head packageTargets).pkg-name; version = (builtins.head packageTargets).pkg-version; id = "${name}-${version}"; };
      components =
        lib.mapAttrs componentsWithPrefix pkgs.haskell-nix.haskellLib.componentPrefix
        // lib.optionalAttrs (componentsByName ? lib) {
          library = lookupComponent "" "" componentsByName.lib;
        };
      checks = pkgs.recurseIntoAttrs (builtins.mapAttrs
        (_: d: pkgs.haskell-nix.haskellLib.check d)
          (lib.filterAttrs (_: d: d.config.doCheck) components.tests));
    };
in {
  hsPkgs =
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
        else redirect packageName packageTargets) (builtins.groupBy (x: x.pkg-name) config.plan-json.targets)) config.preExistingPkgs

    # Redirect for `${name}-${version}`
    // builtins.mapAttrs (packageNameAndVersion: packageTargets: redirect packageNameAndVersion packageTargets)
          (builtins.groupBy (x: "${x.pkg-name}-${x.pkg-version}") config.plan-json.targets);
}
