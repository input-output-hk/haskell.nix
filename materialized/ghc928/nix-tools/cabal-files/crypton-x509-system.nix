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
      identifier = { name = "crypton-x509-system"; version = "1.6.7"; };
      license = "BSD-3-Clause";
      copyright = "Vincent Hanquez <vincent@snarc.org>";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Vincent Hanquez <vincent@snarc.org>";
      homepage = "https://github.com/kazu-yamamoto/crypton-certificate";
      url = "";
      synopsis = "Handle per-operating-system X.509 accessors and storage";
      description = "System X.509 handling for accessing operating system dependents store and other storage methods";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."pem" or (errorHandler.buildDepError "pem"))
          (hsPkgs."crypton-x509" or (errorHandler.buildDepError "crypton-x509"))
          (hsPkgs."crypton-x509-store" or (errorHandler.buildDepError "crypton-x509-store"))
          ] ++ (pkgs.lib).optionals (system.isWindows) [
          (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
          (hsPkgs."asn1-encoding" or (errorHandler.buildDepError "asn1-encoding"))
          ];
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."Crypt32" or (errorHandler.sysDepError "Crypt32"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/crypton-x509-system-1.6.7.tar.gz";
      sha256 = "a436261e5f5e83d85080f57a5509c8224c9e75a6e56d0c43a7d2967052b634ca";
      });
    }) // {
    package-description-override = "Name:                crypton-x509-system\nversion:             1.6.7\nSynopsis:            Handle per-operating-system X.509 accessors and storage\nDescription:         System X.509 handling for accessing operating system dependents store and other storage methods\nLicense:             BSD3\nLicense-file:        LICENSE\nCopyright:           Vincent Hanquez <vincent@snarc.org>\nAuthor:              Vincent Hanquez <vincent@snarc.org>\nMaintainer:          Kazu Yamamoto <kazu@iij.ad.jp>\nBuild-Type:          Simple\nCategory:            Data\nstability:           experimental\nHomepage:            https://github.com/kazu-yamamoto/crypton-certificate\nCabal-Version:       >= 1.10\n\nLibrary\n  Default-Language:  Haskell2010\n  Build-Depends:     base >= 3 && < 5\n                   , bytestring\n                   , mtl\n                   , containers\n                   , directory\n                   , filepath\n                   , process\n                   , pem >= 0.1 && < 0.3\n                   , crypton-x509 >= 1.6\n                   , crypton-x509-store >= 1.6.2\n  Exposed-modules:   System.X509\n                     System.X509.Unix\n                     System.X509.MacOS\n  ghc-options:       -Wall\n  if os(windows)\n     cpp-options:     -DWINDOWS\n     Build-Depends:   Win32, asn1-encoding\n     extra-libraries: Crypt32\n     Exposed-modules: System.X509.Win32\n  if os(OSX)\n     cpp-options: -DMACOSX\n\nsource-repository head\n  type:     git\n  location: https://github.com/kazu-yamamoto/crypton-certificate\n  subdir:   x509-system\n";
    }