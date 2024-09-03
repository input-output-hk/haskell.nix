{ lib, config, pkgs, pkgconfPkgs, haskellLib, ... }:

with lib;
with types;

let
  package = submodule [
    {
      _module.args = {
        inherit pkgs pkgconfPkgs haskellLib;
        inherit (config) hsPkgs errorHandler;
        inherit (config.cabal) system compiler;
      };
    }
    ./package.nix
    # pass down common options as default values
    ({ lib, options, ... }: lib.mkDefault (lib.filterAttrs (n: _v: builtins.hasAttr n options) config))
  ];

in
{
  imports = [
    ./component-options.nix
    ./package-options.nix
  ];

  # Global options. These are passed down to the package level, and from there to the
  # component level, unless specifically overridden.  Depending on the flag flags are
  # combined or replaced. We seed the package Options with an empty set forcing the
  # default values.
  options = {
    use-package-keys = mkOption {
      type = bool;
      default = false;
    };
    package-keys = mkOption {
      type = listOf str;
      default = [];
    };
    packages = if !config.use-package-keys
      then mkOption {
        type = attrsOf package;
      }
      else genAttrs config.package-keys (_n:
          mkOption {
            type = package;
          });
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

    evalPackages = mkOption {
      type = unspecified;
      default = pkgs.pkgsBuildBuild;
      defaultText = "pkgs.pkgsBuildBuild";
      description = ''
        The `evalPackages` that will be used when building `hoogle` and shell tools.
      '';
    };
  };

  config =
  let
    module = config.plan.pkg-def config.hackage.configs;
    addPackageKeys = x: x // { package-keys = builtins.attrNames x.packages; };
  in addPackageKeys {
    inherit (module) compiler;
    packages = lib.mapAttrs (name: { revision, ... }@revArgs: { system, compiler, flags, pkgs, hsPkgs, errorHandler, pkgconfPkgs, ... }@modArgs:

      let m = if revision == null
              then (abort "${name} has no revision!")
              else revision (modArgs // { hsPkgs = hsPkgs // (mapAttrs (l: _: hsPkgs.${name}.components.sublibs.${l}) (m.components.sublibs or {})); });
      in
        m // { flags = lib.mapAttrs (_: lib.mkDefault) (m.flags // revArgs.flags or {});
             }
    ) (lib.filterAttrs (_n: v: v == null || v.revision != null ) module.packages);
  };
}
