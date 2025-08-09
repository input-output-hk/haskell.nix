{ pkgs, lib, haskellLib, ... }:
{
  options = {
    buildable = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    configureFlags = lib.mkOption {
      type = haskellLib.types.listOfFilteringNulls lib.types.str;
      default = [];
    };

    setupBuildFlags = lib.mkOption {
      type = haskellLib.types.listOfFilteringNulls lib.types.str;
      default = [];
    };

    testFlags = lib.mkOption {
      type = haskellLib.types.listOfFilteringNulls lib.types.str;
      default = [];
    };

    setupInstallFlags = lib.mkOption {
      type = haskellLib.types.listOfFilteringNulls lib.types.str;
      default = [];
    };

    setupHaddockFlags = lib.mkOption {
      type = haskellLib.types.listOfFilteringNulls lib.types.str;
      default = [];
    };

    doExactConfig = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    doCheck = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    doCrossCheck = lib.mkOption {
      description = "Run doCheck also in cross compilation settings. This can be tricky as the test logic must know how to run the tests on the target.";
      type = lib.types.bool;
      default = false;
    };

    doHaddock = lib.mkOption {
      description = "Enable building of the Haddock documentation from the annotated Haskell source code.";
      type = lib.types.bool;
      default = true;
    };

    doHoogle = lib.mkOption {
      description = "Also build a hoogle index.";
      type = lib.types.bool;
      default = true;
    };

    doHyperlinkSource = lib.mkOption {
      description = "Link documentation to the source code.";
      type = lib.types.bool;
      default = true;
    };

    doQuickjump = lib.mkOption {
      description = "Generate an index for interactive documentation navigation.";
      type = lib.types.bool;
      default = true;
    };

    doCoverage = lib.mkOption {
      description = "Enable production of test coverage reports.";
      type = lib.types.bool;
      default = false;
    };

    dontPatchELF = lib.mkOption {
      description = "If set, the patchelf command is not used to remove unnecessary RPATH entries. Only applies to Linux.";
      type = lib.types.bool;
      default = true;
    };

    dontStrip = lib.mkOption {
      description = "If set, libraries and executables are not stripped.";
      type = lib.types.bool;
      default = true;
    };

    dontUpdateAutotoolsGnuConfigScripts = lib.mkOption {
      description = "If set, config.sub and config.guess will not be updated.";
      type = lib.types.bool;
      default = false;
    };

    enableDeadCodeElimination = lib.mkOption {
      description = "If set, enables split sections for link-time dead-code stripping. Only applies to Linux";
      type = lib.types.bool;
      default = true;
    };

    enableStatic = lib.mkOption {
      description = "If set, enables building static libraries and executables.";
      type = lib.types.bool;
      # Disabled for ghcjs, see https://gitlab.haskell.org/ghc/ghc/-/issues/23235
      default = !pkgs.stdenv.hostPlatform.isGhcjs && !pkgs.stdenv.hostPlatform.isWasm;
    };

    enableShared = lib.mkOption {
      description = "If set, enables building shared libraries.";
      type = lib.types.bool;
      default = true;
    };

    enableExecutableDynamic = lib.mkOption {
      description = "If set, links executables against shared libraries.";
      type = lib.types.bool;
      default = false;
    };

    configureAllComponents = lib.mkOption {
      description = "If set all the components in the package are configured (useful for cabal-doctest).";
      type = lib.types.bool;
      default = false;
    };

    shellHook = lib.mkOption {
      description = "Hook to run when entering a shell";
      type = lib.types.unspecified; # Can be either a string or a function
      default = "";
    };

    enableLibraryProfiling = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    enableSeparateDataOutput = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    enableLibraryForGhci = lib.mkOption {
      type = lib.types.bool;
      default = true;
    };

    enableProfiling = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };

    profilingDetail = lib.mkOption {
      type = lib.types.nullOr haskellLib.types.uniqueStr;
      default = "default";
    };

    keepConfigFiles = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Keep component configFiles in the store in a `configFiles` output";
    };

    keepGhc = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Keep component wrapped ghc in the store in a `ghc` output";
    };

    keepSource = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Keep component source in the store in a `source` output";
    };

    writeHieFiles = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Write component `.hie` files in the store in a `hie` output";
    };
  };
}
