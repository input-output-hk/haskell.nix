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
      specVersion = "1.18";
      identifier = { name = "safe"; version = "0.3.19"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2007-2020";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://github.com/ndmitchell/safe#readme";
      url = "";
      synopsis = "Library of safe (exception free) functions";
      description = "A library wrapping @Prelude@/@Data.List@ functions that can throw exceptions, such as @head@ and @!!@.\nEach unsafe function has up to four variants, e.g. with @tail@:\n\n* @tail :: [a] -> [a]@, raises an error on @tail []@.\n\n* @tailMay :: [a] -> /Maybe/ [a]@, turns errors into @Nothing@.\n\n* @tailDef :: /[a]/ -> [a] -> [a]@, takes a default to return on errors.\n\n* @tailNote :: /String/ -> [a] -> [a]@, takes an extra argument which supplements the error message.\n\n* @tailSafe :: [a] -> [a]@, returns some sensible default if possible, @[]@ in the case of @tail@.\n\nThis package is divided into three modules:\n\n* \"Safe\" contains safe variants of @Prelude@ and @Data.List@ functions.\n\n* \"Safe.Foldable\" contains safe variants of @Foldable@ functions.\n\n* \"Safe.Exact\" creates crashing versions of functions like @zip@ (errors if the lists are not equal) and @take@ (errors if there are not enough elements), then wraps them to provide safe variants.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        };
      tests = {
        "safe-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."safe" or (errorHandler.buildDepError "safe"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/safe-0.3.19.tar.gz";
      sha256 = "25043442c8f8aa95955bb17467d023630632b961aaa61e807e325d9b2c33f7a2";
      });
    }