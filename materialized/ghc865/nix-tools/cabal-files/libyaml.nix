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
    flags = { no-unicode = false; system-libyaml = false; };
    package = {
      specVersion = "1.12";
      identifier = { name = "libyaml"; version = "0.1.2"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Michael Snoyman <michael@snoyman.com>, Anton Ageev <antage@gmail.com>,Kirill Simonov";
      homepage = "https://github.com/snoyberg/yaml#readme";
      url = "";
      synopsis = "Low-level, streaming YAML interface.";
      description = "README and API documentation are available at <https://www.stackage.org/package/libyaml>";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."directory" or (errorHandler.buildDepError "directory"));
        libs = (pkgs.lib).optional (!(!flags.system-libyaml)) (pkgs."yaml" or (errorHandler.sysDepError "yaml"));
        buildable = true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/libyaml-0.1.2.tar.gz";
      sha256 = "8f42d66f199fcaee255326f8f770d88b0670df56b5eb78002d6058f3a45e97b5";
      });
    }) // {
    package-description-override = "cabal-version: 1.12\r\n\r\n-- This file has been generated from package.yaml by hpack version 0.31.2.\r\n--\r\n-- see: https://github.com/sol/hpack\r\n--\r\n-- hash: 93d917f62be86415287d10db638b1d5422a21b7a4c5b229fbe16b62c47717555\r\n\r\nname:           libyaml\r\nversion:        0.1.2\r\nx-revision: 1\r\nsynopsis:       Low-level, streaming YAML interface.\r\ndescription:    README and API documentation are available at <https://www.stackage.org/package/libyaml>\r\ncategory:       Text\r\nstability:      stable\r\nhomepage:       https://github.com/snoyberg/yaml#readme\r\nbug-reports:    https://github.com/snoyberg/yaml/issues\r\nauthor:         Michael Snoyman <michael@snoyman.com>, Anton Ageev <antage@gmail.com>,Kirill Simonov\r\nmaintainer:     Michael Snoyman <michael@snoyman.com>\r\nlicense:        BSD3\r\nlicense-file:   LICENSE\r\nbuild-type:     Simple\r\nextra-source-files:\r\n    c/helper.h\r\n    libyaml_src/yaml_private.h\r\n    libyaml_src/yaml.h\r\n    libyaml_src/LICENSE\r\n    README.md\r\n    ChangeLog.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/snoyberg/yaml\r\n\r\nflag no-unicode\r\n  description: Don't enable unicode output. Instead, unicode characters will be escaped.\r\n  manual: False\r\n  default: False\r\n\r\nflag system-libyaml\r\n  description: Use the system-wide libyaml instead of the bundled copy\r\n  manual: False\r\n  default: False\r\n\r\nlibrary\r\n  exposed-modules:\r\n      Text.Libyaml\r\n  other-modules:\r\n      Paths_libyaml\r\n  hs-source-dirs:\r\n      src\r\n  ghc-options: -Wall\r\n  include-dirs:\r\n      c\r\n  c-sources:\r\n      c/helper.c\r\n  build-depends:\r\n      base >=4.9.1 && <5\r\n    , bytestring >=0.9.1.4\r\n    , conduit >=1.2.8 && <1.4\r\n    , resourcet >=0.3 && <1.4\r\n  if flag(no-unicode)\r\n    cpp-options: -D__NO_UNICODE__\r\n  if !(flag(system-libyaml))\r\n    include-dirs:\r\n        libyaml_src\r\n    c-sources:\r\n        libyaml_src/api.c\r\n        libyaml_src/dumper.c\r\n        libyaml_src/emitter.c\r\n        libyaml_src/loader.c\r\n        libyaml_src/parser.c\r\n        libyaml_src/reader.c\r\n        libyaml_src/scanner.c\r\n        libyaml_src/writer.c\r\n  else\r\n    extra-libraries:\r\n        yaml\r\n  if os(windows)\r\n    cpp-options: -DWINDOWS\r\n    build-depends:\r\n        directory\r\n  default-language: Haskell2010\r\n";
    }