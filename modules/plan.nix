# The plan (that is, a package set description like an LTS set or a
# plan.nix (derived from plan.json)) will producde a structure that
# looks like, which is stored in config.plan.pkg-def:
#
# { packages = { "package" = { revision = hackageConfigs.$package.$version.revisions.default;
#                              flags = { flag1 = true; flag2 = false; ... }; };
#                ... };
#   compiler = { version = "X.Y.Z"; nix-name ="ghcXYZ";
#                # packages that come bundled with the compiler
#                packages = { "bytestring" = "a.b.c.d"; ... }; };
# }

{ lib, config, pkgs, pkgconfPkgs, haskellLib, ... }:

with lib;
with types;

{
  options = {
    packages = mkOption {
      type = attrsOf (submodule {
        imports = [./package.nix];
        _module.args = {
          inherit pkgs pkgconfPkgs haskellLib;
          inherit (config) hsPkgs;
          inherit (config.cabal) system compiler;
        };
      });
    };

    compiler = {
      version = mkOption {
        type = str;
      };
      nix-name = mkOption {
        type = str;
      };
      packages = mkOption {
        type = attrsOf str;
      };
    };

    plan.pkg-def = mkOption {
      type = unspecified;
      visible = false;
      internal = true;
    };
  };

  config = let module = config.plan.pkg-def config.hackage.configs; in {
    inherit (module) compiler;
    packages = lib.mapAttrs (name: { revision, ... }@revArgs: { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }@modArgs:

      let m = if revision == null
              then (abort "${name} has no revision!")
              else revision (modArgs // { hsPkgs = hsPkgs // (mapAttrs (l: _: hsPkgs.${name}.components.${l})); });
      in
        m // { flags = lib.mapAttrs (_: lib.mkDefault) (m.flags // revArgs.flags or {}); }
    ) (lib.filterAttrs (n: v: v == null || v.revision != null ) module.packages);
  };
}
