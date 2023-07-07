# The plan (that is, a package set description like an LTS set or a
# plan.nix (derived from plan.json)) will produce a structure that
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

let
  inherit (haskellLib.types) getDefaultOrNull listOfFilteringNulls uniqueStr;

  # NOTE:
  # higher up settings work as default for the lower levels
  # so project level doExactConfig sets the default for packages
  # and package doExactConfig sets the default for its components

  packageOptions = def: {
    preUnpack = mkOption {
      type = nullOr lines;
      default = def.preUnpack or null;
    };
    postUnpack = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "postUnpack";
    };
    prePatch = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "prePatch";
    };
    postPatch = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "postPatch";
    };
    preConfigure = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "preConfigure";
    };
    postConfigure = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "postConfigure";
    };
    preBuild = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "preBuild";
    };
    postBuild = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "postBuild";
    };
    preCheck = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "preCheck";
    };
    # Wrapper for test executable run in checkPhase
    testWrapper = mkOption {
      type = listOfFilteringNulls str;
      default = def.testWrapper or [ ];
      description = "A command to run for executing tests in checkPhase, which takes the original test command as its arguments.";
      example = "echo";
    };
    postCheck = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "postCheck";
    };
    preInstall = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "preInstall";
    };
    postInstall = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "postInstall";
    };
    preHaddock = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "preHaddock";
    };
    postHaddock = mkOption {
      type = nullOr uniqueStr;
      default = getDefaultOrNull def "postHaddock";
    };
    hardeningDisable = mkOption {
      type = listOfFilteringNulls str;
      default = def.hardeningDisable or [ ];
    };
    ghcOptions = mkOption {
      type = listOfFilteringNulls str;
      default = def.ghcOptions or [ ];
    };
    contentAddressed = mkOption {
      type = bool;
      default = def.contentAddressed or false;
      description = ''
        Build content addressed derivation, requires Nix to have experimental feature
        `ca-derivations` enabled.
      '';
    };
    planned = mkOption {
      description = "Set to true by `plan-to-nix` for any component that was included in the `plan.json` file.";
      # This is here so that (rather than in componentOptions) so it can be set project wide for stack projects
      type = bool;
      default = def.planned or false;
    };
  };

  package = submodule [
    {
      _module.args = {
        inherit pkgs pkgconfPkgs haskellLib;
        inherit (config) hsPkgs errorHandler;
        inherit (config.cabal) system compiler;
      };
    }
    (import ./package.nix {
      inherit packageOptions;
      parentConfig = config;
    })
    # pass down common options as default values
    ({ lib, options, ... }: lib.mkDefault (lib.filterAttrs (n: _v: builtins.hasAttr n options) config))
  ];

in
{
  imports = [ ./component-options.nix ];

  # Global options. These are passed down to the package level, and from there to the
  # component level, unless specifically overridden.  Depending on the flag flags are
  # combined or replaced. We seed the package Options with an empty set forcing the
  # default values.
  options = (packageOptions { }) // {
    packages = mkOption {
      type = attrsOf package;
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

    evalPackages = mkOption {
      type = unspecified;
      default = pkgs.pkgsBuildBuild;
      defaultText = "pkgs.pkgsBuildBuild";
      description = ''
        The `evalPackages` that will be used when building `hoogle` and shell tools.
      '';
    };
  };

  config = let module = config.plan.pkg-def config.hackage.configs; in {
    inherit (module) compiler;
    packages = lib.mapAttrs (name: { revision, ... }@revArgs: { system, compiler, flags, pkgs, hsPkgs, errorHandler, pkgconfPkgs, ... }@modArgs:

      let m = if revision == null
              then (abort "${name} has no revision!")
              else revision (modArgs // { hsPkgs = hsPkgs // (mapAttrs (l: _: hsPkgs.${name}.components.sublibs.${l}) (m.components.sublibs or {})); });
      in
        m // { flags = lib.mapAttrs (_: lib.mkDefault) (m.flags // revArgs.flags or {});
             }
    ) (lib.filterAttrs (n: v: v == null || v.revision != null ) module.packages);
  };
}
