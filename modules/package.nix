{ lib, options, config, pkgs, haskellLib, ... }:

# Work around issue that can cause _lots_ of files to be copied into the store.
# See https://github.com/NixOS/nixpkgs/pull/64691
let
  inherit (haskellLib.types) listOfFilteringNulls;
  inherit (lib) types;

  path = types.path // { check = x: types.path.check (x.origSrc or x); };

  componentType = types.submodule [
    ./component.nix
    { _module.args = { inherit haskellLib; }; }
    # pass down common options as default values
    ({ lib, options, ... }: lib.mkDefault (lib.filterAttrs (n: _v: builtins.hasAttr n options) config))
  ];

in
{
  imports = [
    ./component-options.nix
    ./package-options.nix
  ];

  # This is how the Nix expressions generated by *-to-nix receive
  # their flags argument.
  config._module.args = { inherit (config) flags; };

  # TODO: Add descriptions to everything.
  options = {
    flags = lib.mkOption {
      type = types.attrsOf types.bool;
    };

    package = {
      specVersion = lib.mkOption {
        type = types.str;
      };

      identifier.name = lib.mkOption {
        type = types.str;
      };

      identifier.version = lib.mkOption {
        type = types.str;
      };

      license = lib.mkOption {
        type = types.str;
      };

      copyright = lib.mkOption {
        type = types.str;
      };

      maintainer = lib.mkOption {
        type = types.str;
      };

      author = lib.mkOption {
        type = types.str;
      };

      homepage = lib.mkOption {
        type = types.str;
      };

      url = lib.mkOption {
        type = types.str;
      };

      synopsis = lib.mkOption {
        type = types.str;
      };

      description = lib.mkOption {
        type = types.str;
      };

      buildType = lib.mkOption {
        type = types.str;
      };

      setup-depends = lib.mkOption {
        type = listOfFilteringNulls types.unspecified;
        default = [ ];
      };

      detailLevel = lib.mkOption {
        type = types.str;
        default = "MinimalDetails";
      };

      licenseFiles = lib.mkOption {
        type = listOfFilteringNulls types.unspecified;
        default = [ ];
      };

      dataDir = lib.mkOption {
        type = types.str;
        default = "";
      };

      dataFiles = lib.mkOption {
        type = listOfFilteringNulls types.unspecified;
        default = [ ];
      };

      extraSrcFiles = lib.mkOption {
        type = listOfFilteringNulls types.unspecified;
        default = [ ];
      };

      extraTmpFiles = lib.mkOption {
        type = listOfFilteringNulls types.unspecified;
        default = [ ];
      };

      extraDocFiles = lib.mkOption {
        type = listOfFilteringNulls types.unspecified;
        default = [ ];
      };

      cleanHpack = lib.mkOption {
        type = types.bool;
        default = false;
      };

      isLocal = lib.mkOption {
        type = types.bool;
        default = false;
      };

      isProject = lib.mkOption {
        type = types.bool;
        default = false;
      };

      buildable = lib.mkOption {
        type = types.bool;
        default = true;
      };
    };

    components = {
      setup = lib.mkOption {
        type = types.nullOr componentType;
        default = {
          depends = [ ];
          libs = [ ];
          frameworks = [ ];
          doExactConfig = false;
          # We have to set hsSourceDirs or cleanCabalComponent will
          # include everything (and as a result all the components of
          # the package will depend on everything in the package).
          # TODO find a better way
          hsSourceDirs = [ "setup-src" ];
          includeDirs = [ ];
          asmSources = [ ];
          cSources = [ ];
          cmmSources = [ ];
          cxxSources = [ ];
          jsSources = [ ];
          extraSrcFiles = [ "Setup.hs" "Setup.lhs" ];
          platforms = null;
        };
      };

      library = lib.mkOption {
        type = types.nullOr componentType;
        default = null;
      };

      sublibs = lib.mkOption {
        type = types.attrsOf componentType;
        default = { };
      };

      foreignlibs = lib.mkOption {
        type = types.attrsOf componentType;
        default = { };
      };

      exes = lib.mkOption {
        type = types.attrsOf componentType;
        default = { };
      };

      tests = lib.mkOption {
        type = types.attrsOf componentType;
        default = { };
      };

      benchmarks = lib.mkOption {
        type = types.attrsOf componentType;
        default = { };
      };
    };

    name = lib.mkOption {
      type = types.str;
      default = "${config.package.identifier.name}-${config.package.identifier.version}";
      defaultText = "\${config.package.identifier.name}-\${config.package.identifier.version}";
    };

    sha256 = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    src = lib.mkOption {
      type = types.nullOr (types.either path types.package);
      default =
        if options.package.identifier.name.isDefined && options.package.identifier.version.isDefined && options.sha256.isDefined
          then
             pkgs.fetchurl {
              url = "mirror://hackage/${config.name}.tar.gz";
              inherit (config) sha256;
            }
          else null;
      defaultText = ''
        pkgs.fetchurl {
          url = "mirror://hackage/$'{config.name}.tar.gz";
          inherit (config) sha256;
        };
      '';
      # Make sure paths have a context so they will be included in the derivation
      # inputs for the component derivations.  Without this sandbox builds fail
      # cannot see the input and fail with the error:
      #   do not know how to unpack source archive /nix/store/...
      apply = v:
        let storeDirMatch = builtins.match "(${builtins.storeDir}/[^/]+).*" v;
        in if builtins.isString v && builtins.getContext v == { } && storeDirMatch != null
        then builtins.appendContext v { ${builtins.head storeDirMatch} = { path = true; }; }
        else v;
    };

    package-description-override = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
      description = "Cabal file to use instead of the one shipped inside the package source distribution.";
    };

    cabal-generator = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    revision = lib.mkOption {
      type = types.nullOr types.int;
      default = null;
    };

    revisionSha256 = lib.mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    patches = lib.mkOption {
      type = types.listOf (types.either types.unspecified path);
      default = [ ];
    };

    # This used to be `components.all` but it has been added back as `allComponent` to
    # to avoid confusion.  It is not mapped by `builder/hspkg-builder.nix` to anything
    # you can build.  Instead it is used internally when `configureAllComponents`
    # is set or for tests whe on `cabal-doctest` is in the `setup-depends` of the package.
    allComponent = lib.mkOption {
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
    in lib.mkMerge
      (
        builtins.map
          (c:
            # Exclude attributes that are likely to have conflicting definitions
            # (a common use case for `all` is in `shellFor` and it only has an
            # install phase).
            builtins.removeAttrs c [ "preCheck" "postCheck" "keepConfigFiles" "keepGhc" "keepSource" ]
          )
          (lib.filter (c: c.buildable && c.planned) allComps)
      ) // {
      # If any one of the components needs us to keep one of these
      # then keep it for the `all` component
      keepConfigFiles = lib.foldl' (x: comp: x || comp.keepConfigFiles) false allComps;
      keepGhc = lib.foldl' (x: comp: x || comp.keepGhc) false allComps;
      keepSource = lib.foldl' (x: comp: x || comp.keepSource) false allComps;
    };
}
