# package descriptions in hackage will look like:
# { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs }:
# { flags = { flag1 = false; flags2 = true; ... };
#   package = { specVersion = "X.Y"; identifier = { name = "..."; version = "a.b.c.d"; };
#               license = "..."; copyright = "..."; maintainer = "..."; author = "...";
#               homepage = "..."; url = "..."; synopsis = "..."; description = "...";
#               buildType = "Simple"; # or Custom, Autoconf, ...
#             };
#  components = {
#    "..." = { depends = [ (hsPkgs.base) ... ]; };
#    exes = { "..." = { depends = ... };
#             "..." = { depends = ... }; };
#    tests = { "..." = { depends = ... }; ... };
#  };

{ parentConfig, mod_args, listOfFilteringNulls, componentOptions, packageOptions }:
{ lib, config, pkgs, haskellLib, ... }:

with lib;
with types;

# Work around issue that can cause _lots_ of files to be copied into the store.
# See https://github.com/NixOS/nixpkgs/pull/64691
let
  path = types.path // { check = x: types.path.check (x.origSrc or x); };

  componentType = submodule {
    # add the shared componentOptions
    options = (packageOptions config) // {
      buildable = mkOption {
        type = bool;
        default = true;
      };
      plugins = mkOption {
        type = listOf (submodule {
          options = {
            library = mkOption {
              type = unspecified;
            };
            moduleName = mkOption {
              type = str;
            };
            args = mkOption {
              type = listOf str;
              default = [];
            };
          };
        });
        default = [];
      };
      depends = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      libs = mkOption {
        type = listOfFilteringNulls (either (nullOr package) (listOfFilteringNulls package));
        default = [];
      };
      frameworks = mkOption {
        type = listOfFilteringNulls package;
        default = [];
      };
      pkgconfig = mkOption {
        type = listOf (listOfFilteringNulls package);
        default = [];
      };
      build-tools = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      modules = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      asmSources = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      cmmSources = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      cSources = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      cxxSources = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      jsSources = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      hsSourceDirs = mkOption {
        type = listOfFilteringNulls unspecified;
        default = ["."];
      };
      includeDirs = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      includes = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      mainPath = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      extraSrcFiles = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };
      platforms = mkOption {
        type = nullOr (listOfFilteringNulls unspecified);
        default = null;
      };
    };
  };

in {
  # This is how the Nix expressions generated by *-to-nix receive
  # their flags argument.
  config._module.args = mod_args // { flags = config.flags; };

  options = (packageOptions parentConfig) // {
    # TODO: Add descriptions to everything.
    flags = mkOption {
      type = attrsOf bool;
    };
    package = {
      specVersion = mkOption {
        type = str;
      };

      identifier.name = mkOption {
        type = str;
      };

      identifier.version = mkOption {
        type = str;
      };

      license = mkOption {
        type = str;
      };

      copyright = mkOption {
        type = str;
      };

      maintainer = mkOption {
        type = str;
      };

      author = mkOption {
        type = str;
      };

      homepage = mkOption {
        type = str;
      };

      url = mkOption {
        type = str;
      };

      synopsis = mkOption {
        type = str;
      };

      description = mkOption {
        type = str;
      };

      buildType = mkOption {
        type = str;
      };

      setup-depends = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };

      detailLevel = mkOption {
        type = str;
        default = "MinimalDetails";
      };

      licenseFiles = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };

      dataDir = mkOption {
        type = str;
        default = "";
      };

      dataFiles = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };

      extraSrcFiles = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };

      extraTmpFiles = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };

      extraDocFiles = mkOption {
        type = listOfFilteringNulls unspecified;
        default = [];
      };

      cleanHpack = mkOption {
        type = bool;
        default = false;
      };

      isLocal = mkOption {
        type = bool;
        default = false;
      };

      isProject = mkOption {
        type = bool;
        default = false;
      };

      buildable = mkOption {
        type = bool;
        default = true;
      };
    };

    components = {
      setup = mkOption {
        type = nullOr componentType;
        default = {
          depends = [];
          libs = [];
          frameworks = [];
          doExactConfig = false;
          # We have to set hsSourceDirs or cleanCabalComponent will
          # include everything (and as a result all the components of
          # the package will depend on everything in the package).
          # TODO find a better way
          hsSourceDirs = ["setup-src"];
          includeDirs = [];
          asmSources = [];
          cSources = [];
          cmmSources = [];
          cxxSources = [];
          jsSources = [];
          extraSrcFiles = [ "Setup.hs" "Setup.lhs" ];
          platforms = null;
        };
      };
      library = mkOption {
        type = nullOr componentType;
        default = null;
      };
      sublibs = mkOption {
        type = attrsOf componentType;
        default = {};
      };
      foreignlibs = mkOption {
        type = attrsOf componentType;
        default = {};
      };
      exes = mkOption {
        type = attrsOf componentType;
        default = {};
      };
      tests = mkOption {
        type = attrsOf componentType;
        default = {};
      };
      benchmarks = mkOption {
        type = attrsOf componentType;
        default = {};
      };
    };

    name = mkOption {
      type = str;
      default = "${config.package.identifier.name}-${config.package.identifier.version}";
      defaultText = "\${config.package.identifier.name}-\${config.package.identifier.version}";
    };
    sha256 = mkOption {
      type = nullOr str;
      default = null;
    };
    src = mkOption {
      type = either path package;
      default = pkgs.fetchurl { url = "mirror://hackage/${config.name}.tar.gz"; inherit (config) sha256; };
      defaultText = "pkgs.fetchurl { url = \"mirror://hackage/\${config.name}.tar.gz\"; inherit (config) sha256; };";
    };
    package-description-override = mkOption {
      type = nullOr str;
      default = null;
      description = "Cabal file to use instead of the one shipped inside the package source distribution.";
    };
    cabal-generator = mkOption {
      type = nullOr str;
      default = null;
    };
    revision = mkOption {
      type = nullOr int;
      default = null;
    };
    revisionSha256 = mkOption {
      type = nullOr str;
      default = null;
    };
    patches = mkOption {
      type = listOf (either unspecified path);
      default = [];
    };
    # This used to be `components.all` but it has been added back as `allComponent` to
    # to avoid confusion.  It is not mapped by `builder/hspkg-builder.nix` to anything
    # you can build.  Instead it is used internally when `configureAllComponents`
    # is set or for tests whe on `cabal-doctest` is in the `setup-depends` of the package.
    allComponent = mkOption {
      type = componentType;
      apply = all: all // {
        # TODO: Should this check for the entire component
        # definition to match, rather than just the identifier?
        depends = builtins.filter (p: p.identifier != config.package.identifier) all.depends;
      };
      description = "The merged dependencies of all other components";
    };
  };

  # This has one quirk. Manually setting options on the all component
  # will be considered a conflict. This is almost always fine; most
  # settings should be modified in either the package options, or an
  # individual component's options. When this isn't sufficient,
  # mkForce is a reasonable workaround.
  #
  # An alternative solution to mkForce for many of the options where
  # this is relevant would be to switch from the bool type to
  # something like an anyBool type, which would merge definitions by
  # returning true if any is true.
  config.allComponent =
    let allComps = haskellLib.getAllComponents config;
    in lib.mkMerge (
      builtins.map (c:
        # Exclude attributes that are likely to have conflicting definitions
        # (a common use case for `all` is in `shellFor` and it only has an
        # install phase).
        builtins.removeAttrs c ["preCheck" "postCheck" "keepSource"]
      ) (lib.filter (c: c.buildable && c.planned) allComps)
    ) // {
      # If any one of the components needs us to keep the source
      # then keep it for the `all` component
      keepSource = lib.foldl' (x: comp: x || comp.keepSource) false allComps;
    };
}
