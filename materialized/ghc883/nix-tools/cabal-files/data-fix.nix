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
      identifier = { name = "data-fix"; version = "0.3.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "<anton.kholomiov@gmail.com>";
      author = "Anton Kholomiov, Edward Kmett, Oleg Grenrus";
      homepage = "https://github.com/spell-music/data-fix";
      url = "";
      synopsis = "Fixpoint data types";
      description = "Fixpoint types and recursion schemes. If you define your AST as\nfixpoint type, you get fold and unfold operations for free.\n\nThanks for contribution to: Matej Kollar, Herbert Valerio Riedel";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          ] ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "7.6") (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))) ++ (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).ge "8.0")) [
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."transformers-compat" or (errorHandler.buildDepError "transformers-compat"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/data-fix-0.3.2.tar.gz";
      sha256 = "3a172d3bc0639c327345e965f9d9023e099425814b28dcdb7b60ff66d66219cc";
      });
    }) // {
    package-description-override = "Name:            data-fix\nVersion:         0.3.2\nx-revision:      3\nCabal-Version:   >= 1.10\nLicense:         BSD3\nLicense-file:    LICENSE\nAuthor:          Anton Kholomiov, Edward Kmett, Oleg Grenrus\nMaintainer:      <anton.kholomiov@gmail.com>\nCategory:        Data\nSynopsis:        Fixpoint data types\nBuild-Type:      Simple\nDescription:\n  Fixpoint types and recursion schemes. If you define your AST as\n  fixpoint type, you get fold and unfold operations for free.\n  .\n  Thanks for contribution to: Matej Kollar, Herbert Valerio Riedel\n\nStability:       Experimental\n\nHomepage:        https://github.com/spell-music/data-fix\nBug-Reports:     https://github.com/spell-music/data-fix/issues\n\nTested-With:\n  GHC ==7.2.2\n   || ==7.4.2\n   || ==7.6.3\n   || ==7.8.4\n   || ==7.10.3\n   || ==8.0.2\n   || ==8.2.2\n   || ==8.4.4\n   || ==8.6.5\n   || ==8.8.4\n   || ==8.10.4\n   || ==9.0.2\n   || ==9.2.4\n   || ==9.4.1\n\nextra-source-files:\n  CHANGELOG.md\n\nSource-repository head\n    Type: git\n    Location: https://github.com/spell-music/data-fix\n\nlibrary\n  hs-source-dirs:   src\n  default-language: Haskell2010\n  ghc-options:      -Wall\n  exposed-modules:  Data.Fix\n\n  if impl(ghc >=8.0)\n    ghc-options: -Wno-trustworthy-safe\n\n  if impl(ghc >=8.4)\n    ghc-options:\n      -Wincomplete-uni-patterns -Wincomplete-record-updates\n      -Wredundant-constraints -Widentities -Wmissing-export-lists\n\n  build-depends:\n      base      >=4.4     && <4.18\n    , deepseq   >=1.3.0.0 && <1.5\n    , hashable  >=1.2.7.0 && <1.5\n\n  if impl(ghc <7.6)\n    -- for GHC.Generics\n    build-depends: ghc-prim\n\n  if !impl(ghc >=8.0)\n    build-depends:\n        transformers         >=0.3   && <0.7\n      , transformers-compat  >=0.6.5 && <0.8\n";
    }