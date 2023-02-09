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
      specVersion = "1.18";
      identifier = { name = "socks"; version = "0.6.1"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Vincent Hanquez <vincent@snarc.org>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "http://github.com/vincenthz/hs-socks";
      url = "";
      synopsis = "Socks proxy (ver 5)";
      description = "Socks proxy (version 5) implementation.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."basement" or (errorHandler.buildDepError "basement"))
          ];
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/socks-0.6.1.tar.gz";
      sha256 = "734447558bb061ce768f53a0df1f2401902c6bee396cc96ce627edd986ef6a73";
      });
    }) // {
    package-description-override = "Name:                socks\nVersion:             0.6.1\nSynopsis:            Socks proxy (ver 5)\nDescription:         Socks proxy (version 5) implementation.\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Vincent Hanquez <vincent@snarc.org>\nBuild-Type:          Simple\nCategory:            Network\nstability:           experimental\nCabal-Version:       1.18\nHomepage:            http://github.com/vincenthz/hs-socks\nextra-doc-files:     README.md, Example.hs\n\nLibrary\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , cereal >= 0.3.1\n                   , network >= 2.6\n                   , basement\n  Exposed-modules:   Network.Socks5\n                     Network.Socks5.Lowlevel\n                     Network.Socks5.Types\n  Other-modules:     Network.Socks5.Wire\n                     Network.Socks5.Conf\n                     Network.Socks5.Command\n                     Network.Socks5.Parse\n  ghc-options:       -Wall -fno-warn-missing-signatures -fwarn-tabs\n  default-language:  Haskell2010\n\nsource-repository head\n  type: git\n  location: git://github.com/vincenthz/hs-socks\n";
    }