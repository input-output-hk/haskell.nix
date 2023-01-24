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
    flags = {
      benchmark-uniplate = false;
      inlining = true;
      dump-splices = false;
      test-hunit = true;
      test-properties = true;
      test-templates = true;
      trustworthy = true;
      j = false;
      };
    package = {
      specVersion = "1.18";
      identifier = { name = "lens"; version = "5.2"; };
      license = "BSD-2-Clause";
      copyright = "Copyright (C) 2012-2016 Edward A. Kmett";
      maintainer = "Edward A. Kmett <ekmett@gmail.com>";
      author = "Edward A. Kmett";
      homepage = "http://github.com/ekmett/lens/";
      url = "";
      synopsis = "Lenses, Folds and Traversals";
      description = "This package comes \\\"Batteries Included\\\" with many useful lenses for the types\ncommonly used from the Haskell Platform, and with tools for automatically\ngenerating lenses and isomorphisms for user-supplied data types.\n\nThe combinators in @Control.Lens@ provide a highly generic toolbox for composing\nfamilies of getters, folds, isomorphisms, traversals, setters and lenses and their\nindexed variants.\n\nAn overview, with a large number of examples can be found in the <https://github.com/ekmett/lens#lens-lenses-folds-and-traversals README>.\n\nAn introductory video on the style of code used in this library by Simon Peyton Jones is available from <http://skillsmatter.com/podcast/scala/lenses-compositional-data-access-and-manipulation Skills Matter>.\n\nA video on how to use lenses and how they are constructed is available on <http://youtu.be/cefnmjtAolY?hd=1 youtube>.\n\nSlides for that second talk can be obtained from <http://comonad.com/haskell/Lenses-Folds-and-Traversals-NYC.pdf comonad.com>.\n\nMore information on the care and feeding of lenses, including a brief tutorial and motivation\nfor their types can be found on the <https://github.com/ekmett/lens/wiki lens wiki>.\n\nA small game of @pong@ and other more complex examples that manage their state using lenses can be found in the <https://github.com/ekmett/lens/blob/master/examples/ example folder>.\n\n/Lenses, Folds and Traversals/\n\nWith some signatures simplified, the core of the hierarchy of lens-like constructions looks like:\n\n\n<<http://i.imgur.com/ALlbPRa.png>>\n\n<images/Hierarchy.png (Local Copy)>\n\nYou can compose any two elements of the hierarchy above using @(.)@ from the @Prelude@, and you can\nuse any element of the hierarchy as any type it linked to above it.\n\nThe result is their lowest upper bound in the hierarchy (or an error if that bound doesn't exist).\n\nFor instance:\n\n* You can use any 'Traversal' as a 'Fold' or as a 'Setter'.\n\n* The composition of a 'Traversal' and a 'Getter' yields a 'Fold'.\n\n/Minimizing Dependencies/\n\nIf you want to provide lenses and traversals for your own types in your own libraries, then you\ncan do so without incurring a dependency on this (or any other) lens package at all.\n\n/e.g./ for a data type:\n\n> data Foo a = Foo Int Int a\n\nYou can define lenses such as\n\n> -- bar :: Lens' (Foo a) Int\n> bar :: Functor f => (Int -> f Int) -> Foo a -> f (Foo a)\n> bar f (Foo a b c) = fmap (\\a' -> Foo a' b c) (f a)\n\n> -- quux :: Lens (Foo a) (Foo b) a b\n> quux :: Functor f => (a -> f b) -> Foo a -> f (Foo b)\n> quux f (Foo a b c) = fmap (Foo a b) (f c)\n\nwithout the need to use any type that isn't already defined in the @Prelude@.\n\nAnd you can define a traversal of multiple fields with 'Control.Applicative.Applicative':\n\n> -- traverseBarAndBaz :: Traversal' (Foo a) Int\n> traverseBarAndBaz :: Applicative f => (Int -> f Int) -> Foo a -> f (Foo a)\n> traverseBarAndBaz f (Foo a b c) = Foo <$> f a <*> f b <*> pure c\n\nWhat is provided in this library is a number of stock lenses and traversals for\ncommon haskell types, a wide array of combinators for working them, and more\nexotic functionality, (/e.g./ getters, setters, indexed folds, isomorphisms).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."assoc" or (errorHandler.buildDepError "assoc"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."base-orphans" or (errorHandler.buildDepError "base-orphans"))
          (hsPkgs."bifunctors" or (errorHandler.buildDepError "bifunctors"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."call-stack" or (errorHandler.buildDepError "call-stack"))
          (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contravariant" or (errorHandler.buildDepError "contravariant"))
          (hsPkgs."distributive" or (errorHandler.buildDepError "distributive"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."free" or (errorHandler.buildDepError "free"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."indexed-traversable" or (errorHandler.buildDepError "indexed-traversable"))
          (hsPkgs."indexed-traversable-instances" or (errorHandler.buildDepError "indexed-traversable-instances"))
          (hsPkgs."kan-extensions" or (errorHandler.buildDepError "kan-extensions"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parallel" or (errorHandler.buildDepError "parallel"))
          (hsPkgs."profunctors" or (errorHandler.buildDepError "profunctors"))
          (hsPkgs."reflection" or (errorHandler.buildDepError "reflection"))
          (hsPkgs."semigroupoids" or (errorHandler.buildDepError "semigroupoids"))
          (hsPkgs."strict" or (errorHandler.buildDepError "strict"))
          (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."th-abstraction" or (errorHandler.buildDepError "th-abstraction"))
          (hsPkgs."these" or (errorHandler.buildDepError "these"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
          ];
        buildable = true;
        };
      tests = {
        "templates" = {
          depends = (pkgs.lib).optionals (!(!flags.test-templates)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            ];
          buildable = if !flags.test-templates then false else true;
          };
        "properties" = {
          depends = (pkgs.lib).optionals (!(!flags.test-properties)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = if !flags.test-properties then false else true;
          };
        "hunit" = {
          depends = (pkgs.lib).optionals (!(!flags.test-hunit)) [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            ];
          buildable = if !flags.test-hunit then false else true;
          };
        "doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."simple-reflect" or (errorHandler.buildDepError "simple-reflect"))
            ];
          buildable = true;
          };
        };
      benchmarks = {
        "plated" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."base-compat" or (errorHandler.buildDepError "base-compat"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ] ++ (pkgs.lib).optional (flags.benchmark-uniplate) (hsPkgs."uniplate" or (errorHandler.buildDepError "uniplate"));
          buildable = true;
          };
        "alongside" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        "folds" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            ];
          buildable = true;
          };
        "traversals" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."vector" or (errorHandler.buildDepError "vector"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            ];
          buildable = true;
          };
        "unsafe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."comonad" or (errorHandler.buildDepError "comonad"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."generic-deriving" or (errorHandler.buildDepError "generic-deriving"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/lens-5.2.tar.gz";
      sha256 = "b33e2ebede468d9e8acb79d20bb5a5947fc3bec13cc39b122aa131c5e6dcd188";
      });
    }) // {
    package-description-override = "name:          lens\ncategory:      Data, Lenses, Generics\nversion:       5.2\nlicense:       BSD2\ncabal-version: 1.18\nlicense-file:  LICENSE\nauthor:        Edward A. Kmett\nmaintainer:    Edward A. Kmett <ekmett@gmail.com>\nstability:     provisional\nhomepage:      http://github.com/ekmett/lens/\nbug-reports:   http://github.com/ekmett/lens/issues\ncopyright:     Copyright (C) 2012-2016 Edward A. Kmett\nbuild-type:    Simple\n-- build-tools:   cpphs\ntested-with:   GHC == 8.0.2\n             , GHC == 8.2.2\n             , GHC == 8.4.4\n             , GHC == 8.6.5\n             , GHC == 8.8.4\n             , GHC == 8.10.7\n             , GHC == 9.0.2\n             , GHC == 9.2.2\nsynopsis:      Lenses, Folds and Traversals\ndescription:\n  This package comes \\\"Batteries Included\\\" with many useful lenses for the types\n  commonly used from the Haskell Platform, and with tools for automatically\n  generating lenses and isomorphisms for user-supplied data types.\n  .\n  The combinators in @Control.Lens@ provide a highly generic toolbox for composing\n  families of getters, folds, isomorphisms, traversals, setters and lenses and their\n  indexed variants.\n  .\n  An overview, with a large number of examples can be found in the <https://github.com/ekmett/lens#lens-lenses-folds-and-traversals README>.\n  .\n  An introductory video on the style of code used in this library by Simon Peyton Jones is available from <http://skillsmatter.com/podcast/scala/lenses-compositional-data-access-and-manipulation Skills Matter>.\n  .\n  A video on how to use lenses and how they are constructed is available on <http://youtu.be/cefnmjtAolY?hd=1 youtube>.\n  .\n  Slides for that second talk can be obtained from <http://comonad.com/haskell/Lenses-Folds-and-Traversals-NYC.pdf comonad.com>.\n  .\n  More information on the care and feeding of lenses, including a brief tutorial and motivation\n  for their types can be found on the <https://github.com/ekmett/lens/wiki lens wiki>.\n  .\n  A small game of @pong@ and other more complex examples that manage their state using lenses can be found in the <https://github.com/ekmett/lens/blob/master/examples/ example folder>.\n  .\n  /Lenses, Folds and Traversals/\n  .\n  With some signatures simplified, the core of the hierarchy of lens-like constructions looks like:\n  .\n  .\n  <<http://i.imgur.com/ALlbPRa.png>>\n  .\n  <images/Hierarchy.png (Local Copy)>\n  .\n  You can compose any two elements of the hierarchy above using @(.)@ from the @Prelude@, and you can\n  use any element of the hierarchy as any type it linked to above it.\n  .\n  The result is their lowest upper bound in the hierarchy (or an error if that bound doesn't exist).\n  .\n  For instance:\n  .\n  * You can use any 'Traversal' as a 'Fold' or as a 'Setter'.\n  .\n  * The composition of a 'Traversal' and a 'Getter' yields a 'Fold'.\n  .\n  /Minimizing Dependencies/\n  .\n  If you want to provide lenses and traversals for your own types in your own libraries, then you\n  can do so without incurring a dependency on this (or any other) lens package at all.\n  .\n  /e.g./ for a data type:\n  .\n  > data Foo a = Foo Int Int a\n  .\n  You can define lenses such as\n  .\n  > -- bar :: Lens' (Foo a) Int\n  > bar :: Functor f => (Int -> f Int) -> Foo a -> f (Foo a)\n  > bar f (Foo a b c) = fmap (\\a' -> Foo a' b c) (f a)\n  .\n  > -- quux :: Lens (Foo a) (Foo b) a b\n  > quux :: Functor f => (a -> f b) -> Foo a -> f (Foo b)\n  > quux f (Foo a b c) = fmap (Foo a b) (f c)\n  .\n  without the need to use any type that isn't already defined in the @Prelude@.\n  .\n  And you can define a traversal of multiple fields with 'Control.Applicative.Applicative':\n  .\n  > -- traverseBarAndBaz :: Traversal' (Foo a) Int\n  > traverseBarAndBaz :: Applicative f => (Int -> f Int) -> Foo a -> f (Foo a)\n  > traverseBarAndBaz f (Foo a b c) = Foo <$> f a <*> f b <*> pure c\n  .\n  What is provided in this library is a number of stock lenses and traversals for\n  common haskell types, a wide array of combinators for working them, and more\n  exotic functionality, (/e.g./ getters, setters, indexed folds, isomorphisms).\n\nextra-source-files:\n  .gitignore\n  .hlint.yaml\n  .vim.custom\n  cabal.project\n  examples/LICENSE\n  examples/lens-examples.cabal\n  examples/*.hs\n  examples/*.lhs\n  examples/.hlint.yaml\n  include/*.h\n  lens-properties/.hlint.yaml\n  lens-properties/CHANGELOG.markdown\n  lens-properties/LICENSE\n  lens-properties/Setup.hs\n  lens-properties/lens-properties.cabal\n  AUTHORS.markdown\n  CHANGELOG.markdown\n  README.markdown\n  SUPPORT.markdown\nextra-doc-files:\n  images/*.png\n\nsource-repository head\n  type: git\n  location: https://github.com/ekmett/lens.git\n\n-- Enable benchmarking against Neil Mitchell's uniplate library for comparative performance analysis. Defaults to being turned off to avoid\n-- the extra dependency.\n--\n-- > cabal configure --enable-benchmarks -fbenchmark-uniplate && cabal build && cabal bench\nflag benchmark-uniplate\n  default: False\n  manual: True\n\n-- Generate inline pragmas when using template-haskell. This defaults to enabled, but you can\n--\n-- > cabal install lens -f-inlining\n--\n-- to shut it off to benchmark the relative performance impact, or as last ditch effort to address compile\n-- errors resulting from the myriad versions of template-haskell that all purport to be 2.8.\nflag inlining\n  manual: True\n  default: True\n\n-- Make the test suites dump their template-haskell splices.\nflag dump-splices\n  default: False\n  manual: True\n\n-- You can disable the hunit test suite with -f-test-hunit\nflag test-hunit\n  default: True\n  manual: True\n\n-- Build the properties test if we're building tests\nflag test-properties\n  default: True\n  manual: True\n\nflag test-templates\n  default: True\n  manual: True\n\n-- Assert that we are trustworthy when we can\nflag trustworthy\n  default: True\n  manual: True\n\n-- Attempt a parallel build with GHC 7.8\nflag j\n  default: False\n  manual: True\n\nlibrary\n  build-depends:\n    array                         >= 0.5.0.0  && < 0.6,\n    assoc                         >= 1.0.2    && < 1.1,\n    base                          >= 4.9      && < 5,\n    base-orphans                  >= 0.5.2    && < 1,\n    bifunctors                    >= 5.5.7    && < 6,\n    bytestring                    >= 0.10.4.0 && < 0.12,\n    call-stack                    >= 0.1      && < 0.5,\n    comonad                       >= 5.0.7    && < 6,\n    containers                    >= 0.5.5.1  && < 0.7,\n    contravariant                 >= 1.4      && < 2,\n    distributive                  >= 0.5.1    && < 1,\n    exceptions                    >= 0.8.2.1  && < 1,\n    filepath                      >= 1.2.0.0  && < 1.5,\n    free                          >= 5.1.5    && < 6,\n    ghc-prim,\n    hashable                      >= 1.2.7.0  && < 1.5,\n    indexed-traversable           >= 0.1      && < 0.2,\n    indexed-traversable-instances >= 0.1      && < 0.2,\n    kan-extensions                >= 5        && < 6,\n    mtl                           >= 2.2.1    && < 2.4,\n    parallel                      >= 3.2.1.0  && < 3.3,\n    profunctors                   >= 5.5.2    && < 6,\n    reflection                    >= 2.1      && < 3,\n    semigroupoids                 >= 5.0.1    && < 6,\n    strict                        >= 0.4      && < 0.5,\n    tagged                        >= 0.8.6    && < 1,\n    template-haskell              >= 2.11.1.0 && < 2.20,\n    text                          >= 1.2.3.0  && < 2.1,\n    th-abstraction                >= 0.4.1    && < 0.5,\n    these                         >= 1.1.1.1  && < 1.2,\n    transformers                  >= 0.5.0.0  && < 0.7,\n    transformers-compat           >= 0.5.0.4  && < 1,\n    unordered-containers          >= 0.2.10   && < 0.3,\n    vector                        >= 0.12.1.2 && < 0.14\n\n  -- Control.Lens as the first module, so cabal repl loads it.\n  exposed-modules:\n    Control.Lens\n\n  exposed-modules:\n    Control.Exception.Lens\n    Control.Lens.At\n    Control.Lens.Combinators\n    Control.Lens.Cons\n    Control.Lens.Each\n    Control.Lens.Empty\n    Control.Lens.Equality\n    Control.Lens.Extras\n    Control.Lens.Fold\n    Control.Lens.Getter\n    Control.Lens.Indexed\n    Control.Lens.Internal\n    Control.Lens.Internal.Bazaar\n    Control.Lens.Internal.ByteString\n    Control.Lens.Internal.Context\n    Control.Lens.Internal.CTypes\n    Control.Lens.Internal.Deque\n    Control.Lens.Internal.Exception\n    Control.Lens.Internal.FieldTH\n    Control.Lens.Internal.PrismTH\n    Control.Lens.Internal.Fold\n    Control.Lens.Internal.Getter\n    Control.Lens.Internal.Indexed\n    Control.Lens.Internal.Instances\n    Control.Lens.Internal.Iso\n    Control.Lens.Internal.Level\n    Control.Lens.Internal.List\n    Control.Lens.Internal.Magma\n    Control.Lens.Internal.Prism\n    Control.Lens.Internal.Profunctor\n    Control.Lens.Internal.Review\n    Control.Lens.Internal.Setter\n    Control.Lens.Internal.TH\n    Control.Lens.Internal.Zoom\n    Control.Lens.Iso\n    Control.Lens.Lens\n    Control.Lens.Level\n    Control.Lens.Operators\n    Control.Lens.Plated\n    Control.Lens.Prism\n    Control.Lens.Profunctor\n    Control.Lens.Reified\n    Control.Lens.Review\n    Control.Lens.Setter\n    Control.Lens.TH\n    Control.Lens.Traversal\n    Control.Lens.Tuple\n    Control.Lens.Type\n    Control.Lens.Unsound\n    Control.Lens.Wrapped\n    Control.Lens.Zoom\n    Control.Monad.Error.Lens\n    Control.Parallel.Strategies.Lens\n    Control.Seq.Lens\n    Data.Array.Lens\n    Data.Bits.Lens\n    Data.ByteString.Lens\n    Data.ByteString.Strict.Lens\n    Data.ByteString.Lazy.Lens\n    Data.Complex.Lens\n    Data.Data.Lens\n    Data.Dynamic.Lens\n    Data.HashSet.Lens\n    Data.IntSet.Lens\n    Data.List.Lens\n    Data.Map.Lens\n    Data.Sequence.Lens\n    Data.Set.Lens\n    Data.Text.Lens\n    Data.Text.Strict.Lens\n    Data.Text.Lazy.Lens\n    Data.Tree.Lens\n    Data.Typeable.Lens\n    Data.Vector.Lens\n    Data.Vector.Generic.Lens\n    GHC.Generics.Lens\n    System.Exit.Lens\n    System.FilePath.Lens\n    System.IO.Error.Lens\n    Language.Haskell.TH.Lens\n    Numeric.Lens\n    Numeric.Natural.Lens\n\n  other-modules:\n    Control.Lens.Internal.Prelude\n\n  if flag(trustworthy) && impl(ghc)\n    other-extensions: Trustworthy\n    cpp-options: -DTRUSTWORTHY=1\n\n  if flag(inlining)\n    cpp-options: -DINLINING\n\n  if flag(j)\n    ghc-options: -j4\n\n  ghc-options: -Wall -Wtabs -O2 -fdicts-cheap -funbox-strict-fields -fmax-simplifier-iterations=10\n               -Wno-trustworthy-safe -Wmissing-pattern-synonym-signatures -Wno-redundant-constraints\n\n  hs-source-dirs: src\n\n  include-dirs: include\n\n  default-language: Haskell2010\n\n  -- future proof, whether the field will be comma separated or not.\n  x-docspec-extra-packages: simple-reflect\n  x-docspec-extra-packages: deepseq\n\n-- Verify that Template Haskell expansion works\ntest-suite templates\n  type: exitcode-stdio-1.0\n  main-is: templates.hs\n  other-modules:\n    T799\n    T917\n    T972\n  ghc-options: -Wall -threaded\n  hs-source-dirs: tests\n  default-language: Haskell2010\n\n  if flag(dump-splices)\n    ghc-options: -ddump-splices\n\n  if !flag(test-templates)\n    buildable: False\n  else\n    build-depends: base, lens\n\n-- Verify the properties of lenses with QuickCheck\ntest-suite properties\n  type: exitcode-stdio-1.0\n  main-is: properties.hs\n  other-modules:\n    Control.Lens.Properties\n  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\n  hs-source-dirs:\n    tests\n    lens-properties/src\n  include-dirs: include\n  default-language: Haskell2010\n  if !flag(test-properties)\n    buildable: False\n  else\n    build-depends:\n      base,\n      lens,\n      QuickCheck                 >= 2.4,\n      test-framework             >= 0.6,\n      test-framework-quickcheck2 >= 0.2,\n      transformers\n\ntest-suite hunit\n  type: exitcode-stdio-1.0\n  main-is: hunit.hs\n  ghc-options: -Wall -threaded -rtsopts -with-rtsopts=-N\n  hs-source-dirs: tests\n  default-language: Haskell2010\n\n  if !flag(test-hunit)\n    buildable: False\n  else\n    build-depends:\n      base,\n      containers,\n      HUnit >= 1.2,\n      lens,\n      mtl,\n      test-framework       >= 0.6,\n      test-framework-hunit >= 0.2\n\n-- We need this dummy test-suite to add simple-reflect to the install plan\n--\n-- When cabal-install's extra-packages support becomes widely available\n-- (i.e. after 3.4 release), we can remove this test-suite.\ntest-suite doctests\n  type:              exitcode-stdio-1.0\n  main-is:           doctests.hs\n  hs-source-dirs:    tests\n  default-language:  Haskell2010\n\n  build-depends: base, deepseq, simple-reflect >= 0.3.1\n\n-- Basic benchmarks for the uniplate-style combinators\nbenchmark plated\n  type:             exitcode-stdio-1.0\n  main-is:          plated.hs\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\n  hs-source-dirs:   benchmarks\n  default-language: Haskell2010\n  build-depends:\n    base,\n    base-compat >=0.11.0 && <0.13,\n    comonad,\n    criterion,\n    deepseq,\n    generic-deriving,\n    lens,\n    transformers\n\n  if flag(benchmark-uniplate)\n    build-depends: uniplate >= 1.6.7 && < 1.7\n    cpp-options: -DBENCHMARK_UNIPLATE\n\n-- Benchmarking alongside variants\nbenchmark alongside\n  type:             exitcode-stdio-1.0\n  main-is:          alongside.hs\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\n  hs-source-dirs:   benchmarks\n  default-language: Haskell2010\n  build-depends:\n    base,\n    comonad >= 4,\n    criterion,\n    deepseq,\n    lens,\n    transformers\n\n-- Benchmarking folds\nbenchmark folds\n  type:             exitcode-stdio-1.0\n  main-is:          folds.hs\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\n  hs-source-dirs:   benchmarks\n  default-language: Haskell2010\n  build-depends:\n    base,\n    criterion,\n    containers,\n    bytestring,\n    unordered-containers,\n    vector,\n    lens\n\n-- Benchmarking traversals\nbenchmark traversals\n  type:             exitcode-stdio-1.0\n  main-is:          traversals.hs\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\n  hs-source-dirs:   benchmarks\n  default-language: Haskell2010\n  build-depends:\n    base,\n    criterion,\n    containers,\n    deepseq,\n    bytestring,\n    unordered-containers,\n    vector,\n    lens\n\n-- Benchmarking unsafe implementation strategies\nbenchmark unsafe\n  type:             exitcode-stdio-1.0\n  main-is:          unsafe.hs\n  ghc-options:      -Wall -O2 -threaded -fdicts-cheap -funbox-strict-fields\n  hs-source-dirs:   benchmarks\n  default-language: Haskell2010\n  build-depends:\n    base,\n    comonad >= 4,\n    criterion >= 1,\n    deepseq,\n    generic-deriving,\n    lens,\n    transformers\n";
    }