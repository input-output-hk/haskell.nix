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
    flags = { binary = true; parsec = true; attoparsec = true; };
    package = {
      specVersion = "1.10";
      identifier = { name = "parsers"; version = "0.12.10"; };
      license = "BSD-3-Clause";
      copyright = "Copyright (C) 2010-2013 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/parsers/";
      url = "";
      synopsis = "Parsing combinators";
      description = "This library provides convenient combinators for working with and building parsing combinator libraries.\n\nGiven a few simple instances, e.g. for the class 'Text.Parser.Combinators.Parsing' in \"Text.Parser.Combinators.Parsing\" you\nget access to a large number of canned definitions. Instances exist for the parsers provided by @parsec@,\n@attoparsec@ and base’s \"Text.Read\".";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."charset" or (errorHandler.buildDepError "charset"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."scientific" or (errorHandler.buildDepError "scientific"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          ] ++ (pkgs.lib).optional (flags.binary) (hsPkgs."binary" or (errorHandler.buildDepError "binary"))) ++ (pkgs.lib).optional (flags.parsec) (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))) ++ (pkgs.lib).optional (flags.attoparsec) (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"));
        buildable = true;
        };
      tests = {
        "quickcheck" = {
          depends = ([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."parsers" or (errorHandler.buildDepError "parsers"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."quickcheck-instances" or (errorHandler.buildDepError "quickcheck-instances"))
            ] ++ (pkgs.lib).optional (flags.parsec) (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))) ++ (pkgs.lib).optional (flags.attoparsec) (hsPkgs."attoparsec" or (errorHandler.buildDepError "attoparsec"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/parsers-0.12.10.tar.gz";
      sha256 = "17b91f1318ca54679395b382a056df633fdb44fbb962eca66b1787f957af1a6c";
      });
    }) // {
    package-description-override = "name:          parsers\ncategory:      Text, Parsing\nversion:       0.12.10\nlicense:       BSD3\ncabal-version: >= 1.10\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     experimental\nhomepage:      http://github.com/ekmett/parsers/\nbug-reports:   http://github.com/ekmett/parsers/issues\ncopyright:     Copyright (C) 2010-2013 Edward A. Kmett\nsynopsis:      Parsing combinators\ndescription:\n  This library provides convenient combinators for working with and building parsing combinator libraries.\n  .\n  Given a few simple instances, e.g. for the class 'Text.Parser.Combinators.Parsing' in \"Text.Parser.Combinators.Parsing\" you\n  get access to a large number of canned definitions. Instances exist for the parsers provided by @parsec@,\n  @attoparsec@ and base’s \"Text.Read\".\nbuild-type:    Simple\ntested-with:   GHC==7.0.4\n             , GHC==7.2.2\n             , GHC==7.4.2\n             , GHC==7.6.3\n             , GHC==7.8.4\n             , GHC==7.10.3\n             , GHC==8.0.2\n             , GHC==8.2.2\n             , GHC==8.4.4\n             , GHC==8.6.5\n             , GHC==8.8.1\n\nextra-source-files:\n  .travis.yml\n  CHANGELOG.markdown\n  README.markdown\n  HLint.hs\n\nsource-repository head\n  type: git\n  location: git://github.com/ekmett/parsers.git\n\nflag binary\n  default: True\n  description:\n    You can disable the use of the `binary` package using `-f-binary`.\n\nflag parsec\n  default: True\n  description:\n    You can disable the use of the `parsec` package using `-f-parsec`.\n\nflag attoparsec\n  default: True\n  description:\n    You can disable the use of the `attoparsec` package using `-f-attoparsec`.\n\nlibrary\n  default-language: Haskell2010\n  exposed-modules:\n    Text.Parser.Char\n    Text.Parser.Combinators\n    Text.Parser.LookAhead\n    Text.Parser.Permutation\n    Text.Parser.Expression\n    Text.Parser.Token\n    Text.Parser.Token.Style\n    Text.Parser.Token.Highlight\n\n  hs-source-dirs: src\n\n  ghc-options: -Wall -fno-warn-wrong-do-bind -fwarn-monomorphism-restriction -fwarn-incomplete-record-updates\n  if impl(ghc >= 7.2)\n    ghc-options: -fwarn-identities -fwarn-incomplete-uni-patterns\n  if impl(ghc >= 7.10)\n    ghc-options: -fno-warn-trustworthy-safe\n\n  build-depends:\n    base                 >= 4.3      && < 5,\n    base-orphans         >= 0.3      && < 1,\n    charset              >= 0.3      && < 1,\n    containers           >= 0.4      && < 0.7,\n    semigroups           >= 0.12     && < 1,\n    text                 >= 0.10     && < 1.3,\n    transformers         >= 0.2      && < 0.6,\n    mtl                  >= 2.0.1    && < 2.3,\n    scientific           >= 0.3      && < 0.4,\n    unordered-containers >= 0.2      && < 0.3\n\n  if flag(binary)\n    build-depends: binary     >= 0.7.2    && < 1\n  if flag(parsec)\n    build-depends: parsec     >= 3.1      && < 3.2\n  if flag(attoparsec)\n    build-depends: attoparsec >= 0.12.1.4 && < 0.14\n\ntest-suite quickcheck\n  type:    exitcode-stdio-1.0\n  main-is: QuickCheck.hs\n  default-language: Haskell2010\n  build-depends:\n    base == 4.*,\n    bytestring,\n    parsers,\n    QuickCheck,\n    quickcheck-instances\n  ghc-options: -Wall -threaded\n  hs-source-dirs: tests\n\n  if flag(parsec)\n    build-depends: parsec >= 3\n  if flag(attoparsec)\n    build-depends: attoparsec\n";
    }