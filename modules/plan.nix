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
  # dealing with str is a bit annoying especially with `nullOr str` as that apparently defaults to ""
  # instead of null :shrug:.  This then messes with our option inheritance logic.
  # Hence we have a uniqueStr type that ensures multiple identically defined options are collapsed
  # without raising an error. And a way to fetch default options that will retain `null` if the
  # option is not defined or "".
  getDefaultOrNull = def: key: if def ? ${key} && def.${key} != "" then def.${key} else null;
  mergeUniqueOption = locs: defs: let
    mergeOneOption = loc: defs':
      # we ignore "" as optionalString, will default to "".
      let defs = filter (x: x.value != "") defs'; in
      if defs == [] then null
      else if length defs != 1 then
        throw "The unique option `${showOption loc}' is defined multiple times, in ${showFiles (getFiles defs)}; with values `${concatStringsSep "', `" (map (x: x.value) defs)}'."
      else (head defs).value;
  in mergeOneOption locs (lists.unique defs);
  uniqueStr = str // { merge = mergeUniqueOption; };

  # This is just like listOf, except that it filters out all null elements.
  listOfFilteringNulls = elemType: listOf elemType // {
    # Mostly copied from nixpkgs/lib/types.nix
    merge = loc: defs:
      map (x: x.value) (filter (x: x ? value && x.value != null) (concatLists (imap1 (n: def:
        if isList def.value then
          imap1 (m: def':
            (mergeDefinitions
              (loc ++ ["[definition ${toString n}-entry ${toString m}]"])
              elemType
              [{ inherit (def) file; value = def'; }]
            ).optionalValue
          ) def.value
        else
          throw "The option value `${showOption loc}` in `${def.file}` is not a list.") defs)));
  };

  componentOptions = def: {
         buildable = mkOption {
            type = bool;
            default = true;
         };
         configureFlags = mkOption {
            type = listOfFilteringNulls str;
            default = (def.configureFlags or []);
          };
          setupBuildFlags = mkOption {
            type = listOfFilteringNulls str;
            default = (def.setupBuildFlags or []);
          };
          testFlags = mkOption {
            type = listOfFilteringNulls str;
            default = (def.testFlags or []);
          };
          setupInstallFlags = mkOption {
            type = listOfFilteringNulls str;
            default = (def.setupInstallFlags or []);
          };
          setupHaddockFlags = mkOption {
            type = listOfFilteringNulls str;
            default = (def.setupHaddockFlags or []);
          };
          doExactConfig = mkOption {
            type = bool;
            default = (def.doExactConfig or false);
          };
          doCheck = mkOption {
            type = bool;
            default = (def.doCheck or true);
          };
          doCrossCheck = mkOption {
            description = "Run doCheck also in cross compilation settings. This can be tricky as the test logic must know how to run the tests on the target.";
            type = bool;
            default = (def.doCrossCheck or false);
          };
          doHaddock = mkOption {
            description = "Enable building of the Haddock documentation from the annotated Haskell source code.";
            type = bool;
            default = (def.doHaddock or true);
          };
          doHoogle = mkOption {
            description = "Also build a hoogle index.";
            type = bool;
            default = (def.doHoogle or true);
          };
          doHyperlinkSource = mkOption {
            description = "Link documentation to the source code.";
            type = bool;
            default = (def.doHyperlinkSource or true);
          };
          doQuickjump = mkOption {
            description = "Generate an index for interactive documentation navigation.";
            type = bool;
            default = (def.doQuickjump or true);
          };
          doCoverage = mkOption {
            description = "Enable production of test coverage reports.";
            type = bool;
            default = (def.doCoverage or false);
          };
          dontPatchELF = mkOption {
            description = "If set, the patchelf command is not used to remove unnecessary RPATH entries. Only applies to Linux.";
            type = bool;
            default = (def.dontPatchELF or true);
          };
          dontStrip = mkOption {
            description = "If set, libraries and executables are not stripped.";
            type = bool;
            default = (def.dontStrip or true);
          };
          enableDeadCodeElimination = mkOption {
            description = "If set, enables split sections for link-time dead-code stripping. Only applies to Linux";
            type = bool;
            default = (def.enableDeadCodeElimination or true);
          };
          enableStatic = mkOption {
            description = "If set, enables building static libraries and executables.";
            type = bool;
            default = (def.enableStatic or true);
          };
          enableShared = mkOption {
            description = "If set, enables building shared libraries.";
            type = bool;
            default = (def.enableShared or true);
          };
          configureAllComponents = mkOption {
            description = "If set all the components in the package are configured (useful for cabal-doctest).";
            type = bool;
            default = false;
          };
          shellHook = mkOption {
            description = "Hook to run when entering a shell";
            type = unspecified; # Can be either a string or a function
            default = (def.shellHook or "");
          };
    enableLibraryProfiling = mkOption {
      type = bool;
      default = (def.enableLibraryProfiling or false);
    };

    enableSeparateDataOutput = mkOption {
      type = bool;
      default = (def.enableSeparateDataOutput or true);
    };

    enableProfiling = mkOption {
      type = bool;
      default = (def.enableProfiling or false);
    };

    profilingDetail = mkOption {
      type = nullOr uniqueStr;
      default = (def.profilingDetail or "default");
    };

    keepConfigFiles = mkOption {
      type = bool;
      default = (def.keepConfigFiles or false);
      description = "Keep component configFiles in the store in a `configFiles` output";
    };

    keepGhc = mkOption {
      type = bool;
      default = (def.keepGhc or false);
      description = "Keep component wrapped ghc in the store in a `ghc` output";
    };

    keepSource = mkOption {
      type = bool;
      default = (def.keepSource or false);
      description = "Keep component source in the store in a `source` output";
    };

    writeHieFiles = mkOption {
      type = bool;
      default = (def.writeHieFiles or false);
      description = "Write component `.hie` files in the store in a `hie` output";
    };
  };
  packageOptions = def: componentOptions def // {
    preUnpack = mkOption {
      type = nullOr lines;
      default = (def.preUnpack or null);
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
      default = def.testWrapper or [];
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
      default = (def.hardeningDisable or []);
    };
    ghcOptions = mkOption {
      type = listOfFilteringNulls str;
      default = def.ghcOptions or [];
    };
    contentAddressed = mkOption {
      type = bool;
      default = (def.contentAddressed or false);
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


in {
  # Global options. These are passed down to the package level, and from there to the
  # component level, unless specifically overridden.  Depending on the flag flags are
  # combined or replaced. We seed the package Options with an empty set forcing the
  # default values.
  options = (packageOptions {}) // {

    packages = mkOption {
      type =
        let mod_args = {
          inherit pkgs pkgconfPkgs haskellLib;
          inherit (config) hsPkgs errorHandler;
          inherit (config.cabal) system compiler;
        }; in
          attrsOf (submodule (import ./package.nix {
            inherit mod_args listOfFilteringNulls;
            inherit componentOptions packageOptions;
            parentConfig = config;
           }));
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
