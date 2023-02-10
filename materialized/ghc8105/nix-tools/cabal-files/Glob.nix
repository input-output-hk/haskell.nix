{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "Glob"; version = "0.10.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Matti Niemenmaa <matti.niemenmaa+glob@iki.fi>";
      author = "Matti Niemenmaa";
      homepage = "http://iki.fi/matti.niemenmaa/glob/";
      url = "";
      synopsis = "Globbing library";
      description = "A library for globbing: matching patterns against file paths.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        };
      tests = {
        "glob-tests" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."dlist" or (errorHandler.buildDepError "dlist"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))) ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/Glob-0.10.2.tar.gz";
      sha256 = "6af672ac8427d35cbd42d64142dc288feab266f0be92dae3c696e8860d8173c0";
      });
    }) // {
    package-description-override = "Cabal-Version: >= 1.10\r\n\r\nName:        Glob\r\nVersion:     0.10.2\r\nx-revision:  3\r\nHomepage:    http://iki.fi/matti.niemenmaa/glob/\r\nSynopsis:    Globbing library\r\nCategory:    System\r\nStability:   provisional\r\nDescription:\r\n   A library for globbing: matching patterns against file paths.\r\n\r\nAuthor:       Matti Niemenmaa\r\nMaintainer:   Matti Niemenmaa <matti.niemenmaa+glob@iki.fi>\r\nLicense:      BSD3\r\nLicense-File: LICENSE.txt\r\n\r\nBuild-Type: Simple\r\n\r\nExtra-Source-Files: CHANGELOG.txt\r\n                    CREDITS.txt\r\n                    README.txt\r\n\r\nSource-Repository head\r\n  Type: git\r\n  Location: https://github.com/Deewiant/glob\r\n\r\nLibrary\r\n   Build-Depends: base                >= 4 && < 5\r\n                , containers\r\n                , directory\r\n                , dlist               >= 0.4\r\n                , filepath            >= 1.1\r\n                , transformers        >= 0.2\r\n                , transformers-compat >= 0.3\r\n\r\n   if impl(ghc < 8.0)\r\n      Build-Depends: semigroups >= 0.18\r\n\r\n   if os(windows)\r\n      Build-Depends: Win32 >= 2.5\r\n\r\n   Default-Language: Haskell98\r\n\r\n   Exposed-Modules: System.FilePath.Glob\r\n                    System.FilePath.Glob.Primitive\r\n   Other-Modules:   System.FilePath.Glob.Base\r\n                    System.FilePath.Glob.Directory\r\n                    System.FilePath.Glob.Match\r\n                    System.FilePath.Glob.Simplify\r\n                    System.FilePath.Glob.Utils\r\n\r\n   GHC-Options: -Wall\r\n\r\nTest-Suite glob-tests\r\n   type: exitcode-stdio-1.0\r\n\r\n   hs-source-dirs: ., tests\r\n   main-is: Main.hs\r\n\r\n   Build-Depends: base                       >= 4 && < 5\r\n                , containers\r\n                , directory\r\n                , dlist                      >= 0.4\r\n                , filepath                   >= 1.1\r\n                , transformers               >= 0.2\r\n                , transformers-compat        >= 0.3\r\n                , HUnit                      >= 1.2\r\n                , QuickCheck                 >= 2\r\n                , test-framework             >= 0.2\r\n                , test-framework-hunit       >= 0.2\r\n                , test-framework-quickcheck2 >= 0.3\r\n\r\n   if impl(ghc < 8.0)\r\n      Build-Depends: semigroups >= 0.18\r\n\r\n   if os(windows)\r\n      Build-Depends: Win32 >= 2\r\n\r\n   Default-Language: Haskell98\r\n\r\n   Other-Modules: System.FilePath.Glob.Base\r\n                  System.FilePath.Glob.Directory\r\n                  System.FilePath.Glob.Match\r\n                  System.FilePath.Glob.Primitive\r\n                  System.FilePath.Glob.Simplify\r\n                  System.FilePath.Glob.Utils\r\n                  Tests.Base\r\n                  Tests.Compiler\r\n                  Tests.Directory\r\n                  Tests.Instances\r\n                  Tests.Matcher\r\n                  Tests.Optimizer\r\n                  Tests.Regression\r\n                  Tests.Simplifier\r\n                  Tests.Utils\r\n\r\n   GHC-Options: -Wall";
    }