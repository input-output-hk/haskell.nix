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
      identifier = { name = "unix-time"; version = "0.4.10"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Kazu Yamamoto <kazu@iij.ad.jp>";
      author = "Kazu Yamamoto <kazu@iij.ad.jp>";
      homepage = "";
      url = "";
      synopsis = "Unix time parser/formatter and utilities";
      description = "Fast parser\\/formatter\\/utilities for Unix time";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          ];
        libs = (pkgs.lib).optionals (system.isWindows) ((pkgs.lib).optional (compiler.isGhc && (compiler.version).ge "9.4.5" && !(compiler.isGhc && (compiler.version).ge "9.4.6") || compiler.isGhc && (compiler.version).ge "9.6.1" && !(compiler.isGhc && (compiler.version).ge "9.6.3")) (pkgs."mingwex" or (errorHandler.sysDepError "mingwex")));
        build-tools = [
          (hsPkgs.buildPackages.hsc2hs.components.exes.hsc2hs or (pkgs.buildPackages.hsc2hs or (errorHandler.buildToolDepError "hsc2hs:hsc2hs")))
          ];
        buildable = true;
        };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."old-locale" or (errorHandler.buildDepError "old-locale"))
            (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."unix-time" or (errorHandler.buildDepError "unix-time"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            ];
          build-tools = [
            (hsPkgs.buildPackages.hspec-discover.components.exes.hspec-discover or (pkgs.buildPackages.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/unix-time-0.4.10.tar.gz";
      sha256 = "7e4a4bf8cba87bc7b1e3a1a7c68a44d7780360d6f52628cd84154829a03629bc";
      });
    }) // {
    package-description-override = "cabal-version:      1.18\nname:               unix-time\nversion:            0.4.10\nlicense:            BSD3\nlicense-file:       LICENSE\nmaintainer:         Kazu Yamamoto <kazu@iij.ad.jp>\nauthor:             Kazu Yamamoto <kazu@iij.ad.jp>\nsynopsis:           Unix time parser/formatter and utilities\ndescription:        Fast parser\\/formatter\\/utilities for Unix time\ncategory:           Data\nbuild-type:         Configure\nextra-source-files:\n    cbits/config.h.in\n    cbits/conv.c\n    cbits/strftime.c\n    cbits/strptime.c\n    cbits/win_patch.c\n    cbits/win_patch.h\n    configure\n    configure.ac\n\nextra-tmp-files:\n    config.log\n    config.status\n    autom4te.cache\n    cbits/config.h\n\nsource-repository head\n    type:     git\n    location: https://github.com/kazu-yamamoto/unix-time\n\nlibrary\n    exposed-modules:  Data.UnixTime\n    build-tools:      hsc2hs >=0\n    c-sources:        cbits/conv.c\n    other-modules:\n        Data.UnixTime.Conv\n        Data.UnixTime.Diff\n        Data.UnixTime.Types\n        Data.UnixTime.Sys\n\n    default-language: Haskell2010\n    include-dirs:     cbits\n    ghc-options:      -Wall\n    build-depends:\n        base >=4 && <5,\n        bytestring,\n        old-time,\n        binary\n\n    if impl(ghc >=7.8)\n        cc-options: -fPIC\n\n    -- GHC 9.4.5, 9.6.1, and 9.6.2 on Windows do not link against mingwex, but\n    -- unix-time implicitly depends on this library due to the use of the\n    -- gettimeofday() function, which comes from mingwex on Windows. To avoid\n    -- linker errors in the absence of a mingwex dependency (see\n    -- https://gitlab.haskell.org/ghc/ghc/-/issues/23533 for an example of\n    -- this), we depend on mingwex explicitly here.\n    --\n    -- Other versions of GHC on Windows already depend on mingwex, so we guard\n    -- this behind appropriate conditionals.\n    if os(windows)\n        if (impl(ghc >= 9.4.5) && !impl(ghc >= 9.4.6)) || (impl(ghc >= 9.6.1) && !impl(ghc >= 9.6.3))\n            extra-libraries: mingwex\n\n    if os(windows)\n        c-sources:\n            cbits/strftime.c\n            cbits/strptime.c\n            cbits/win_patch.c\n\ntest-suite spec\n    type:             exitcode-stdio-1.0\n    main-is:          Spec.hs\n    build-tools:      hspec-discover >=2.6\n    hs-source-dirs:   test\n    other-modules:    UnixTimeSpec\n    default-language: Haskell2010\n    ghc-options:      -Wall\n    build-depends:\n        base,\n        bytestring,\n        old-locale,\n        old-time,\n        QuickCheck,\n        template-haskell,\n        time,\n        unix-time,\n        hspec >=2.6\n";
    }