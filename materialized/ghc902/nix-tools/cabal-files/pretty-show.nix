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
      identifier = { name = "pretty-show"; version = "1.10"; };
      license = "MIT";
      copyright = "";
      maintainer = "iavor.diatchki@gmail.com";
      author = "Iavor S. Diatchki";
      homepage = "http://wiki.github.com/yav/pretty-show";
      url = "";
      synopsis = "Tools for working with derived `Show` instances and generic\ninspection of values.";
      description = "We provide a library and an executable for working with derived 'Show'\ninstances. By using the library, we can parse derived 'Show' instances into a\ngeneric data structure. The @ppsh@ tool uses the library to produce\nhuman-readable versions of 'Show' instances, which can be quite handy for\ndebugging Haskell programs.  We can also render complex generic values into\nan interactive Html page, for easier examination.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."haskell-lexer" or (errorHandler.buildDepError "haskell-lexer"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.happy.components.exes.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy:happy")))
          (hsPkgs.buildPackages.happy.components.exes.happy or (pkgs.buildPackages.happy or (errorHandler.buildToolDepError "happy:happy")))
          ];
        buildable = true;
        };
      exes = {
        "ppsh" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/pretty-show-1.10.tar.gz";
      sha256 = "307f9086e0b063d439dc4f513e36a145e8a57f23de448aefae2a6c00f6da6fd2";
      });
    }) // {
    package-description-override = "name:           pretty-show\nversion:        1.10\ncategory:       Text\n\nsynopsis:       Tools for working with derived `Show` instances and generic\n                inspection of values.\ndescription:\n  We provide a library and an executable for working with derived 'Show'\n  instances. By using the library, we can parse derived 'Show' instances into a\n  generic data structure. The @ppsh@ tool uses the library to produce\n  human-readable versions of 'Show' instances, which can be quite handy for\n  debugging Haskell programs.  We can also render complex generic values into\n  an interactive Html page, for easier examination.\n\nlicense:        MIT\nlicense-file:   LICENSE\nauthor:         Iavor S. Diatchki\nmaintainer:     iavor.diatchki@gmail.com\n\nhomepage:       http://wiki.github.com/yav/pretty-show\n\ncabal-version:  >= 1.8\nbuild-type:     Simple\n\ntested-with:    GHC == 7.10.3\n                GHC == 8.0.2\n                GHC == 8.2.2\n                GHC == 8.4.4\n                GHC == 8.6.1\n\ndata-files:\n  style/jquery.js\n  style/pretty-show.js\n  style/pretty-show.css\n\nextra-source-files:\n  CHANGELOG\n\nlibrary\n  exposed-modules:\n    Text.Show.Pretty\n  other-modules:\n    Text.Show.Html\n    Text.Show.Parser\n    Text.Show.Value\n    Text.Show.PrettyVal\n    Paths_pretty_show\n  build-depends:\n    array          >= 0.2  &&  < 2,\n    base           >= 4.5  &&  < 5,\n    haskell-lexer  >= 1.1    &&  < 2,\n    pretty         >= 1    &&  < 2,\n    text,\n    filepath,\n    ghc-prim\n  ghc-options: -Wall -O2\n  if impl(ghc < 7.4)\n    cpp-options: -DNO_GENERICS\n  build-tool-depends: happy:happy\n  build-tools: happy\n\nexecutable ppsh\n  main-is: ppsh.hs\n  other-modules: Paths_pretty_show\n\n  hs-source-dirs: bin\n  build-depends:\n    base          >= 4.5  &&  < 5,\n    pretty-show\n  ghc-options: -Wall\n\nsource-repository head\n  type:     git\n  location: https://github.com/yav/pretty-show.git\n\n\n";
    }