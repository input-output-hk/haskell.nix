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
    flags = { buildreadme = false; text = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "prettyprinter"; version = "1.7.1"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "Simon Jakobi <simon.jakobi@gmail.com>, David Luposchainsky <dluposchainsky at google>";
      author = "Phil Wadler, Daan Leijen, Max Bolingbroke, Edward Kmett, David Luposchainsky, Simon Jakobi";
      homepage = "http://github.com/quchen/prettyprinter";
      url = "";
      synopsis = "A modern, easy to use, well-documented, extensible pretty-printer.";
      description = "A modern, easy to use, well-documented, extensible pretty-printer. For more see README.md";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (pkgs.lib).optional (flags.text) (hsPkgs."text" or (errorHandler.buildDepError "text"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."fail" or (errorHandler.buildDepError "fail"))
          ]) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.10")) (hsPkgs."void" or (errorHandler.buildDepError "void"));
        buildable = true;
        };
      exes = {
        "generate_readme" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ];
          buildable = if flags.buildreadme && flags.text then true else false;
          };
        };
      tests = {
        "doctest" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = if compiler.isGhc && (compiler.version).lt "7.10"
            then false
            else true;
          };
        "testsuite" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."pgp-wordlist" or (errorHandler.buildDepError "pgp-wordlist"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = if !flags.text then false else true;
          };
        };
      benchmarks = {
        "fusion" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            ];
          buildable = if !flags.text then false else true;
          };
        "faster-unsafe-text" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            ];
          buildable = if !flags.text then false else true;
          };
        "large-output" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
            (hsPkgs."ansi-wl-pprint" or (errorHandler.buildDepError "ansi-wl-pprint"))
            (hsPkgs."gauge" or (errorHandler.buildDepError "gauge"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            ] ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "7.6")) (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optional (!(compiler.isGhc && (compiler.version).ge "8.0")) (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = if !flags.text then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/prettyprinter-1.7.1.tar.gz";
      sha256 = "5e6ea6903114fa118fcc359633dfb7ecddecb92c06c853d02a77b72b251f0b45";
      });
    }) // {
    package-description-override = "name:                prettyprinter\nversion:             1.7.1\ncabal-version:       >= 1.10\ncategory:            User Interfaces, Text\nsynopsis:            A modern, easy to use, well-documented, extensible pretty-printer.\ndescription:         A modern, easy to use, well-documented, extensible pretty-printer. For more see README.md\nlicense:             BSD2\nlicense-file:        LICENSE.md\nextra-source-files:  README.md\n                   , CHANGELOG.md\n                   , misc/version-compatibility-macros.h\nauthor:              Phil Wadler, Daan Leijen, Max Bolingbroke, Edward Kmett, David Luposchainsky, Simon Jakobi\nmaintainer:          Simon Jakobi <simon.jakobi@gmail.com>, David Luposchainsky <dluposchainsky at google>\nbug-reports:         http://github.com/quchen/prettyprinter/issues\nhomepage:            http://github.com/quchen/prettyprinter\nbuild-type:          Simple\ntested-with:         GHC==9.0.1, GHC==8.10.4, GHC==8.8.4, GHC==8.6.5, GHC==8.4.4, GHC==8.2.2, GHC==8.0.2, GHC==7.10.3, GHC==7.8.4, GHC==7.6.3, GHC==7.4.2\n\nsource-repository head\n    type: git\n    location: git://github.com/quchen/prettyprinter.git\n\n\n\nlibrary\n    exposed-modules:\n          Prettyprinter\n        , Prettyprinter.Internal\n        , Prettyprinter.Internal.Debug\n        , Prettyprinter.Internal.Type\n        , Prettyprinter.Render.String\n        , Prettyprinter.Render.Text\n        , Prettyprinter.Render.Tutorials.StackMachineTutorial\n        , Prettyprinter.Render.Tutorials.TreeRenderingTutorial\n        , Prettyprinter.Render.Util.Panic\n        , Prettyprinter.Render.Util.SimpleDocTree\n        , Prettyprinter.Render.Util.StackMachine\n        , Prettyprinter.Util\n\n        , Prettyprinter.Symbols.Unicode\n        , Prettyprinter.Symbols.Ascii\n\n        , Data.Text.Prettyprint.Doc\n        , Data.Text.Prettyprint.Doc.Internal\n        , Data.Text.Prettyprint.Doc.Internal.Debug\n        , Data.Text.Prettyprint.Doc.Internal.Type\n        , Data.Text.Prettyprint.Doc.Render.String\n        , Data.Text.Prettyprint.Doc.Render.Text\n        , Data.Text.Prettyprint.Doc.Render.Tutorials.StackMachineTutorial\n        , Data.Text.Prettyprint.Doc.Render.Tutorials.TreeRenderingTutorial\n        , Data.Text.Prettyprint.Doc.Render.Util.Panic\n        , Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree\n        , Data.Text.Prettyprint.Doc.Render.Util.StackMachine\n        , Data.Text.Prettyprint.Doc.Util\n\n        , Data.Text.Prettyprint.Doc.Symbols.Unicode\n        , Data.Text.Prettyprint.Doc.Symbols.Ascii\n\n    ghc-options: -Wall -O2\n    hs-source-dirs: src\n    include-dirs: misc\n    default-language: Haskell2010\n    other-extensions:\n          BangPatterns\n        , CPP\n        , OverloadedStrings\n        , DefaultSignatures\n        , ScopedTypeVariables\n\n    build-depends:\n          base >= 4.5 && < 5\n\n    if flag(text)\n        build-depends: text >= 1.2\n    else\n        -- A fake text package, emulating the same API, but backed by String\n        hs-source-dirs: src-text\n        other-modules:\n              Data.Text\n            , Data.Text.IO\n            , Data.Text.Lazy\n            , Data.Text.Lazy.Builder\n\n    if !impl(ghc >= 7.6)\n        build-depends: ghc-prim\n\n    if impl(ghc >= 8.0)\n        ghc-options: -Wcompat\n    if !impl(ghc >= 8.0)\n        build-depends: semigroups >= 0.17\n        build-depends: fail >= 4.9.0.0 && <4.10\n    if !impl(ghc >= 7.10)\n        build-depends: void >=0.4 && <0.8\n\n\n\nFlag buildReadme\n  Description: Build the readme generator\n  Default:     False\n\nFlag text\n  Description: While it's a core value of @prettyprinter@ to use @Text@, there are rare\n               circumstances (mostly when @prettyprinter@ arises as a dependency of\n               test suites of packages like @bytestring@ or @text@ themselves) when\n               this is inconvenient. In this case one can disable this flag, so that\n               @prettyprinter@ fallbacks to @String@.\n  Default:     True\n\n\nexecutable generate_readme\n    hs-source-dirs: app\n    main-is: GenerateReadme.hs\n    build-depends:\n          base >= 4.7 && < 5\n        , prettyprinter\n\n        , text\n        , template-haskell >= 2.9\n    default-language: Haskell2010\n    other-modules: MultilineTh\n    other-extensions: OverloadedStrings\n                    , TemplateHaskell\n                    , QuasiQuotes\n    if flag(buildReadme) && flag(text)\n        buildable: True\n    else\n        buildable: False\n\n\n\ntest-suite doctest\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test/Doctest\n    main-is: Main.hs\n    build-depends:\n          base       >= 4.7 && < 5\n        , doctest    >= 0.9\n        , prettyprinter\n        , QuickCheck >= 2.5\n    ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\n    default-language: Haskell2010\n    if impl (ghc < 7.10)\n        buildable: False\n        -- Doctest does not support searching through directories in old versions\n\ntest-suite testsuite\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test/Testsuite\n    main-is: Main.hs\n    other-modules: StripTrailingSpace\n    build-depends:\n          base\n        , prettyprinter\n\n        , pgp-wordlist     >= 0.1\n        , bytestring\n        , quickcheck-instances >= 0.3\n        , tasty            >= 0.10\n        , tasty-hunit      >= 0.9\n        , tasty-quickcheck >= 0.8\n        , text\n    ghc-options: -threaded -rtsopts -with-rtsopts=-N -Wall\n    default-language: Haskell2010\n\n    if !impl(ghc >= 8.0)\n        build-depends: semigroups >= 0.6\n\n    if !flag(text)\n        buildable: False\n\n\nbenchmark fusion\n    type: exitcode-stdio-1.0\n    hs-source-dirs: bench\n    main-is: Fusion.hs\n    build-depends:\n          base >= 4.5 && < 5\n        , prettyprinter\n\n        , gauge          >= 0.2\n        , mtl            >= 2.1\n        , random         >= 1.0\n        , text\n        , transformers   >= 0.3\n        , ansi-wl-pprint >= 0.6\n    ghc-options: -Wall -rtsopts\n    default-language: Haskell2010\n    other-extensions: OverloadedStrings\n\n    if !flag(text)\n        buildable: False\n\nbenchmark faster-unsafe-text\n    build-depends:\n          base >= 4.5 && < 5\n        , prettyprinter\n\n        , gauge >= 0.2\n        , text\n\n    hs-source-dirs:      bench\n    main-is:             FasterUnsafeText.hs\n    ghc-options:         -rtsopts -Wall\n    default-language:    Haskell2010\n    type:                exitcode-stdio-1.0\n\n    if !flag(text)\n        buildable: False\n\nbenchmark large-output\n    build-depends:\n          base >= 4.5 && < 5\n        , base-compat >=0.9.3 && <0.12\n        , prettyprinter\n        , ansi-wl-pprint\n\n        , gauge >= 0.2\n        , QuickCheck >= 2.7\n        , containers\n        , text\n        , deepseq\n\n    hs-source-dirs:      bench\n    main-is:             LargeOutput.hs\n    ghc-options:         -rtsopts -Wall\n    default-language:    Haskell2010\n    type:                exitcode-stdio-1.0\n\n    if !impl(ghc >= 7.6)\n        build-depends: ghc-prim\n\n    if !impl(ghc >= 8.0)\n        build-depends: semigroups\n\n    if !flag(text)\n        buildable: False\n";
    }