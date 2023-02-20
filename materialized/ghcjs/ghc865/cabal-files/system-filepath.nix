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
      specVersion = "1.8";
      identifier = { name = "system-filepath"; version = "0.4.14"; };
      license = "MIT";
      copyright = "John Millikin 2010-2012";
      maintainer = "FP Complete <michael@fpcomplete.com>";
      author = "John Millikin <jmillikin@gmail.com>";
      homepage = "https://github.com/fpco/haskell-filesystem";
      url = "";
      synopsis = "High-level, byte-based file and directory path manipulations (deprecated)";
      description = "Please see: https://plus.google.com/+MichaelSnoyman/posts/Ft5hnPqpgEx";
      buildType = "Custom";
      setup-depends = [
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        ];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
        buildable = true;
        };
      tests = {
        "filesystem_path_tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."chell" or (errorHandler.buildDepError "chell"))
            (hsPkgs."chell-quickcheck" or (errorHandler.buildDepError "chell-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."system-filepath" or (errorHandler.buildDepError "system-filepath"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/system-filepath-0.4.14.tar.gz";
      sha256 = "1656ce3c0d585650784ceb3f794748286e19fb635f557e7b29b0897f8956d993";
      });
    }) // {
    package-description-override = "name: system-filepath\nversion: 0.4.14\nx-revision: 1\nsynopsis: High-level, byte-based file and directory path manipulations (deprecated)\ndescription: Please see: https://plus.google.com/+MichaelSnoyman/posts/Ft5hnPqpgEx\nlicense: MIT\nlicense-file: license.txt\nauthor: John Millikin <jmillikin@gmail.com>\nmaintainer: FP Complete <michael@fpcomplete.com>\ncopyright: John Millikin 2010-2012\nbuild-type: Custom\ncabal-version: >= 1.8\ncategory: System\nstability: experimental\nhomepage: https://github.com/fpco/haskell-filesystem\nbug-reports: https://github.com/fpco/haskell-filesystem/issues\nextra-source-files: README.md ChangeLog.md\n\nsource-repository head\n  type: git\n  location: https://github.com/fpco/haskell-filesystem.git\n\ncustom-setup\n  setup-depends: Cabal >= 1.8, base >=4.0 && <5\n\nlibrary\n  ghc-options: -Wall -O2\n  hs-source-dirs: lib\n\n  build-depends:\n      base >= 4.0 && < 5.0\n    , bytestring >= 0.9\n    , deepseq >= 1.1 && < 1.5\n    , text >= 0.11.0.6\n  if !impl(ghc >= 8.0)\n    build-depends:\n      semigroups >= 0.11 && < 0.19\n\n  if os(windows)\n    cpp-options: -DCABAL_OS_WINDOWS\n\n  if os(darwin)\n    cpp-options: -DCABAL_OS_DARWIN\n\n  exposed-modules:\n    Filesystem.Path\n    Filesystem.Path.CurrentOS\n    Filesystem.Path.Rules\n\n  other-modules:\n    Filesystem.Path.Internal\n\ntest-suite filesystem_path_tests\n  type: exitcode-stdio-1.0\n  main-is: FilesystemPathTests.hs\n\n  ghc-options: -Wall -O2\n  cc-options: -Wall\n  hs-source-dirs: tests\n\n  build-depends:\n      base > 4.0 && < 5.0\n    , bytestring\n    , chell >= 0.4 && < 0.5\n    , chell-quickcheck >= 0.2 && < 0.3\n    , QuickCheck\n    , system-filepath\n    , text\n";
    }