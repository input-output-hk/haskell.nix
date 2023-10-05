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
      specVersion = "1.12";
      identifier = { name = "attoparsec-aeson"; version = "2.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2011-2016 Bryan O'Sullivan\n(c) 2011 MailRank, Inc.";
      maintainer = "Oleg Grenrus <oleg.grenrus@iki.fi>";
      author = "Bryan O'Sullivan <bos@serpentine.com>";
      homepage = "https://github.com/haskell/aeson";
      url = "";
      synopsis = "Parsing of aeson's Value with attoparsec";
      description = "Parsing of aeson's Value with attoparsec, originally from aeson.\n\nThis version is an empty package with bounds on @aeson@\nwhich has \"Data.Aeson.Parser\" module.\nThis way e.g. specifying\n\n@\nbuild-depends: aeson >= 1.4.1.0 && \\<2.3, attoparsec-aeson >=2.1.0.0 && \\<2.3\n@\n\nwill have \"Data.Aeson.Parser\" available for all @aeson@ versions in range.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/attoparsec-aeson-2.1.0.0.tar.gz";
      sha256 = "b34afb1c330428d9ff7e9e99655ece826ed7a1928dd5880c8127e73a12e906a9";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\nname:          attoparsec-aeson\nversion:       2.1.0.0\nsynopsis:      Parsing of aeson's Value with attoparsec\ndescription:\n  Parsing of aeson's Value with attoparsec, originally from aeson.\n  .\n  This version is an empty package with bounds on @aeson@\n  which has \"Data.Aeson.Parser\" module.\n  This way e.g. specifying\n  .\n  @\n  build-depends: aeson >= 1.4.1.0 && \\<2.3, attoparsec-aeson >=2.1.0.0 && \\<2.3\n  @\n  .\n  will have \"Data.Aeson.Parser\" available for all @aeson@ versions in range.\n\nlicense:       BSD3\nlicense-file:  LICENSE\ncategory:      Parsing\ncopyright:\n  (c) 2011-2016 Bryan O'Sullivan\n  (c) 2011 MailRank, Inc.\n\nauthor:        Bryan O'Sullivan <bos@serpentine.com>\nmaintainer:    Oleg Grenrus <oleg.grenrus@iki.fi>\nstability:     experimental\nhomepage:      https://github.com/haskell/aeson\nbug-reports:   https://github.com/haskell/aeson/issues\nbuild-type:    Simple\n\nlibrary\n  default-language: Haskell2010\n  build-depends:\n      aeson               >=1.4.1.0 && <2.2\n    , base                <5\n\nsource-repository head\n  type:     git\n  location: git://github.com/haskell/aeson.git\n  subdir:   attoparsec-aeson\n";
    }