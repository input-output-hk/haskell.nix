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
    flags = { examples = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "MemoTrie"; version = "0.6.10"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2008-2019 by Conal Elliott";
      maintainer = "conal@conal.net";
      author = "Conal Elliott ";
      homepage = "https://github.com/conal/MemoTrie";
      url = "";
      synopsis = "Trie-based memo functions";
      description = "MemoTrie provides a basis for memoized functions over some domains,\nusing tries.  It's based on ideas from Ralf Hinze and code from\nSpencer Janssen. Generic support thanks to Sam Boosalis.\n\nProject wiki page: <http://haskell.org/haskellwiki/MemoTrie>\n\n&#199; 2008-2019 by Conal Elliott; BSD3 license.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = if compiler.isGhc && (compiler.version).ge "7.10.0"
          then [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            ]
          else [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."void" or (errorHandler.buildDepError "void"))
            (hsPkgs."newtype-generics" or (errorHandler.buildDepError "newtype-generics"))
            ];
        buildable = true;
        };
      exes = {
        "generic" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."MemoTrie" or (errorHandler.buildDepError "MemoTrie"))
            ];
          buildable = if !flags.examples then false else true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/MemoTrie-0.6.10.tar.gz";
      sha256 = "584df0e138093b2f2edc893a69883eb8cbca3402ebdc75392a7742e86156ba53";
      });
    }) // {
    package-description-override = "Name:                MemoTrie\nVersion:             0.6.10\nCabal-Version:       >= 1.10\nSynopsis:            Trie-based memo functions\nCategory:            Data\nDescription:\n  MemoTrie provides a basis for memoized functions over some domains,\n  using tries.  It's based on ideas from Ralf Hinze and code from\n  Spencer Janssen. Generic support thanks to Sam Boosalis.\n  .\n  Project wiki page: <http://haskell.org/haskellwiki/MemoTrie>\n  .\n  &#199; 2008-2019 by Conal Elliott; BSD3 license.\nHomepage:            https://github.com/conal/MemoTrie\nAuthor:              Conal Elliott \nMaintainer:          conal@conal.net\nCopyright:           (c) 2008-2019 by Conal Elliott\nLicense:             BSD3\nLicense-File:        COPYING\nStability:           experimental\nbuild-type:          Simple\n\nsource-repository head\n  type:     git\n  location: git://github.com/conal/MemoTrie.git\n\nFlag examples\n  Description: \"examples\"\n  Default:     False\n  Manual:      True\n\nLibrary\n  hs-Source-Dirs:      src\n\n  if impl(ghc >= 7.10.0)\n     Build-Depends: base >=4.8.0.0 && <5, newtype-generics >= 0.5.3\n  else\n     Build-Depends: base <4.8.0.0, void, newtype-generics >= 0.5.3\n\n  Exposed-Modules:     \n                     Data.MemoTrie\n  Other-Modules:     \n\n  default-language: Haskell2010\n\nexecutable generic \n  if !flag(examples)\n    buildable:         False\n  main-is:             Generic.hs\n  ghc-options:         -Wall\n  hs-source-dirs:      examples\n  default-language:    Haskell2010\n  build-depends:       base, MemoTrie\n\n";
    }