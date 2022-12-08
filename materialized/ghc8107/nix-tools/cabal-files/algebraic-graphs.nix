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
    flags = {};
    package = {
      specVersion = "2.2";
      identifier = { name = "algebraic-graphs"; version = "0.5"; };
      license = "MIT";
      copyright = "Andrey Mokhov, 2016-2019";
      maintainer = "Andrey Mokhov <andrey.mokhov@gmail.com>, github: @snowleopard,\nAlexandre Moine <alexandre@moine.me>, github: @nobrakal";
      author = "Andrey Mokhov <andrey.mokhov@gmail.com>, github: @snowleopard";
      homepage = "https://github.com/snowleopard/alga";
      url = "";
      synopsis = "A library for algebraic graph construction and transformation";
      description = "<https://github.com/snowleopard/alga Alga> is a library for algebraic construction and\nmanipulation of graphs in Haskell. See <https://github.com/snowleopard/alga-paper this paper>\nfor the motivation behind the library, the underlying theory and implementation details.\n\nThe top-level module\n<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph.html Algebra.Graph>\ndefines the main data type for /algebraic graphs/\n<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph.html#t:Graph Graph>,\nas well as associated algorithms. For type-safe representation and\nmanipulation of /non-empty algebraic graphs/, see\n<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph-NonEmpty.html Algebra.Graph.NonEmpty>.\nFurthermore, /algebraic graphs with edge labels/ are implemented in\n<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph-Labelled.html Algebra.Graph.Labelled>.\n\nThe library also provides conventional graph data structures, such as\n<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph-AdjacencyMap.html Algebra.Graph.AdjacencyMap>\nalong with its various flavours: adjacency maps specialised to graphs with\nvertices of type 'Int'\n(<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph-AdjacencyIntMap.html Algebra.Graph.AdjacencyIntMap>),\nnon-empty adjacency maps\n(<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph-NonEmpty-AdjacencyMap.html Algebra.Graph.NonEmpty.AdjacencyMap>),\nand adjacency maps with edge labels\n(<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph-Labelled-AdjacencyMap.html Algebra.Graph.Labelled.AdjacencyMap>).\nA large part of the API of algebraic graphs and adjacency maps is available\nthrough the 'Foldable'-like type class\n<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph-ToGraph.html Algebra.Graph.ToGraph>.\n\nThe type classes defined in\n<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph-Class.html Algebra.Graph.Class>\nand\n<http://hackage.haskell.org/package/algebraic-graphs/docs/Algebra-Graph-HigherKinded-Class.html Algebra.Graph.HigherKinded.Class>\ncan be used for polymorphic construction and manipulation of graphs.\n\nThis is an experimental library and the API is expected to remain unstable until version 1.0.0.\nPlease consider contributing to the on-going\n<https://github.com/snowleopard/alga/issues discussions on the library API>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
        buildable = true;
        };
      tests = {
        "test-alga" = {
          depends = [
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."algebraic-graphs" or (errorHandler.buildDepError "algebraic-graphs"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."inspection-testing" or (errorHandler.buildDepError "inspection-testing"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/algebraic-graphs-0.5.tar.gz";
      sha256 = "89b9fecf8245476ec823355125fcb95decf41fd9784e807d7bd0d09f0a79c50b";
      });
    }