{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { threaded = true; selftest = true; };
    package = {
      specVersion = "1.18";
      identifier = { name = "hadrian"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "Andrey Mokhov 2014-2017";
      maintainer = "Andrey Mokhov <andrey.mokhov@gmail.com>, github: @snowleopard";
      author = "Andrey Mokhov <andrey.mokhov@gmail.com>, github: @snowleopard";
      homepage = "";
      url = "";
      synopsis = "GHC build system";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [ "README.md" ];
    };
    components = {
      exes = {
        "hadrian" = {
          depends = [
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
            (hsPkgs."shake" or (errorHandler.buildDepError "shake"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ pkgs.lib.optional (flags.selftest) (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"));
          buildable = true;
          modules = [
            "Base"
            "Builder"
            "CommandLine"
            "Context"
            "Context/Path"
            "Context/Type"
            "Environment"
            "Expression"
            "Expression/Type"
            "Flavour"
            "Flavour/Type"
            "Hadrian/Builder"
            "Hadrian/Builder/Ar"
            "Hadrian/Builder/Sphinx"
            "Hadrian/Builder/Tar"
            "Hadrian/Builder/Git"
            "Hadrian/BuildPath"
            "Hadrian/Expression"
            "Hadrian/Haskell/Cabal"
            "Hadrian/Haskell/Cabal/Type"
            "Hadrian/Haskell/Cabal/Parse"
            "Hadrian/Oracles/ArgsHash"
            "Hadrian/Oracles/Cabal"
            "Hadrian/Oracles/Cabal/Rules"
            "Hadrian/Oracles/Cabal/Type"
            "Hadrian/Oracles/DirectoryContents"
            "Hadrian/Oracles/Path"
            "Hadrian/Oracles/TextFile"
            "Hadrian/Package"
            "Hadrian/Target"
            "Hadrian/Utilities"
            "Oracles/Flag"
            "Oracles/Flavour"
            "Oracles/Setting"
            "Oracles/ModuleFiles"
            "Oracles/TestSettings"
            "Packages"
            "Rules"
            "Rules/BinaryDist"
            "Rules/CabalReinstall"
            "Rules/Clean"
            "Rules/Compile"
            "Rules/Dependencies"
            "Rules/Docspec"
            "Rules/Documentation"
            "Rules/Generate"
            "Rules/Gmp"
            "Rules/Libffi"
            "Rules/Library"
            "Rules/Lint"
            "Rules/Nofib"
            "Rules/Program"
            "Rules/Register"
            "Rules/Rts"
            "Rules/SimpleTargets"
            "Rules/SourceDist"
            "Rules/Test"
            "Rules/ToolArgs"
            "Settings"
            "Settings/Builders/Alex"
            "Settings/Builders/Cabal"
            "Settings/Builders/Common"
            "Settings/Builders/Cc"
            "Settings/Builders/Configure"
            "Settings/Builders/DeriveConstants"
            "Settings/Builders/GenPrimopCode"
            "Settings/Builders/Ghc"
            "Settings/Builders/GhcPkg"
            "Settings/Builders/Haddock"
            "Settings/Builders/Happy"
            "Settings/Builders/Hsc2Hs"
            "Settings/Builders/HsCpp"
            "Settings/Builders/Ar"
            "Settings/Builders/Ld"
            "Settings/Builders/Make"
            "Settings/Builders/MergeObjects"
            "Settings/Builders/RunTest"
            "Settings/Builders/Win32Tarballs"
            "Settings/Builders/Xelatex"
            "Settings/Default"
            "Settings/Flavours/Benchmark"
            "Settings/Flavours/Development"
            "Settings/Flavours/GhcInGhci"
            "Settings/Flavours/Performance"
            "Settings/Flavours/Quick"
            "Settings/Flavours/QuickCross"
            "Settings/Flavours/Quickest"
            "Settings/Flavours/Validate"
            "Settings/Flavours/Release"
            "Settings/Packages"
            "Settings/Parser"
            "Settings/Program"
            "Settings/Warnings"
            "Stage"
            "Target"
            "UserSettings"
            "Utilities"
            "Way"
            "Way/Type"
          ] ++ pkgs.lib.optional (flags.selftest) "Rules/Selftest";
          hsSourceDirs = [ "." "src" ];
          mainPath = ([
            "Main.hs"
          ] ++ pkgs.lib.optional (flags.threaded) "") ++ pkgs.lib.optional (flags.selftest) "";
        };
      };
    };
  } // rec { src = pkgs.lib.mkDefault ../.; }