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
      specVersion = "3.0";
      identifier = { name = "time"; version = "1.12.2"; };
      license = "BSD-2-Clause";
      copyright = "";
      maintainer = "<ashley@semantic.org>";
      author = "Ashley Yakeley";
      homepage = "https://github.com/haskell/time";
      url = "";
      synopsis = "A time library";
      description = "Time, clocks and calendars";
      buildType = "Configure";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"));
        buildable = true;
        };
      tests = {
        "ShowDefaultTZAbbreviations" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        "ShowTime" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            ];
          buildable = true;
          };
        "test-main" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = true;
          };
        "test-unix" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            ];
          buildable = if system.isWindows then false else true;
          };
        };
      benchmarks = {
        "time-bench" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."criterion" or (errorHandler.buildDepError "criterion"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/time-1.12.2.tar.gz";
      sha256 = "dba0b047a230e823ce08007b4a6c2cd0a1e9e899c148b72019511c0b71ebaf1e";
      });
    }) // {
    package-description-override = "cabal-version:  3.0\nname:           time\nversion:        1.12.2\nstability:      stable\nlicense:        BSD-2-Clause\nlicense-file:   LICENSE\nauthor:         Ashley Yakeley\nmaintainer:     <ashley@semantic.org>\nhomepage:       https://github.com/haskell/time\nbug-reports:    https://github.com/haskell/time/issues\nsynopsis:       A time library\ndescription:    Time, clocks and calendars\ncategory:       Time\nbuild-type:     Configure\ntested-with:\n    GHC == 8.10.7,\n    GHC == 9.0.2,\n    GHC == 9.2.2\nx-follows-version-policy:\n\nextra-source-files:\n    changelog.md\n    aclocal.m4\n    configure.ac\n    configure\n    lib/include/HsTime.h\n    lib/include/HsTimeConfig.h.in\n    test/unix/Test/Format/*.c\n    test/unix/Test/Format/*.h\nextra-tmp-files:\n    config.log\n    config.status\n    autom4te.cache\n    lib/include/HsTimeConfig.h\n\nsource-repository head\n    type:     git\n    location: https://github.com/haskell/time\n\nlibrary\n    hs-source-dirs: lib\n    default-language: Haskell2010\n    default-extensions:\n        Rank2Types\n        DeriveDataTypeable\n        StandaloneDeriving\n        PatternSynonyms\n        ViewPatterns\n    ghc-options: -Wall -fwarn-tabs\n    c-sources: lib/cbits/HsTime.c\n    build-depends:\n        base >= 4.14 && < 5,\n        deepseq >= 1.1\n    if os(windows)\n        build-depends: Win32\n    exposed-modules:\n        Data.Time.Calendar,\n        Data.Time.Calendar.MonthDay,\n        Data.Time.Calendar.OrdinalDate,\n        Data.Time.Calendar.WeekDate,\n        Data.Time.Calendar.Julian,\n        Data.Time.Calendar.Easter,\n        Data.Time.Calendar.Month,\n        Data.Time.Calendar.Quarter,\n        Data.Time.Clock,\n        Data.Time.Clock.System,\n        Data.Time.Clock.POSIX,\n        Data.Time.Clock.TAI,\n        Data.Time.LocalTime,\n        Data.Time.Format,\n        Data.Time.Format.Internal,\n        Data.Time.Format.ISO8601,\n        Data.Time\n    other-modules:\n        Data.Format,\n        Data.Time.Calendar.CalendarDiffDays,\n        Data.Time.Calendar.Days,\n        Data.Time.Calendar.Gregorian,\n        Data.Time.Calendar.JulianYearDay,\n        Data.Time.Calendar.Private,\n        Data.Time.Calendar.Types,\n        Data.Time.Calendar.Week,\n        Data.Time.Clock.Internal.DiffTime,\n        Data.Time.Clock.Internal.AbsoluteTime,\n        Data.Time.Clock.Internal.NominalDiffTime,\n        Data.Time.Clock.Internal.POSIXTime,\n        Data.Time.Clock.Internal.UniversalTime,\n        Data.Time.Clock.Internal.SystemTime,\n        Data.Time.Clock.Internal.UTCTime,\n        Data.Time.Clock.Internal.CTimeval,\n        Data.Time.Clock.Internal.CTimespec,\n        Data.Time.Clock.Internal.UTCDiff,\n        Data.Time.LocalTime.Internal.TimeZone,\n        Data.Time.LocalTime.Internal.TimeOfDay,\n        Data.Time.LocalTime.Internal.CalendarDiffTime,\n        Data.Time.LocalTime.Internal.LocalTime,\n        Data.Time.LocalTime.Internal.ZonedTime,\n        Data.Time.Format.Parse,\n        Data.Time.Format.Locale,\n        Data.Time.Format.Format.Class,\n        Data.Time.Format.Format.Instances,\n        Data.Time.Format.Parse.Class,\n        Data.Time.Format.Parse.Instances\n    include-dirs: lib/include\n    if os(windows)\n        install-includes:\n            HsTime.h\n    else\n        autogen-includes:\n            HsTimeConfig.h\n        install-includes:\n            HsTime.h\n            HsTimeConfig.h\n\ntest-suite ShowDefaultTZAbbreviations\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test\n    default-language: Haskell2010\n    ghc-options: -Wall -fwarn-tabs\n    build-depends:\n        base,\n        time\n    main-is: ShowDefaultTZAbbreviations.hs\n\ntest-suite ShowTime\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test\n    default-language: Haskell2010\n    ghc-options: -Wall -fwarn-tabs\n    build-depends:\n        base,\n        time\n    main-is: ShowTime.hs\n\ntest-suite test-main\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test/main\n    default-language: Haskell2010\n    default-extensions:\n        Rank2Types\n        GeneralizedNewtypeDeriving\n        DeriveDataTypeable\n        StandaloneDeriving\n        ExistentialQuantification\n        MultiParamTypeClasses\n        FlexibleInstances\n        UndecidableInstances\n        ScopedTypeVariables\n        TupleSections\n    ghc-options: -Wall -fwarn-tabs\n    build-depends:\n        base,\n        deepseq,\n        time,\n        random,\n        QuickCheck,\n        tasty,\n        tasty-hunit,\n        tasty-quickcheck\n    main-is: Main.hs\n    other-modules:\n        Test.Types\n        Test.TestUtil\n        Test.Arbitrary\n        Test.Calendar.AddDays\n        Test.Calendar.AddDaysRef\n        Test.Calendar.CalendarProps\n        Test.Calendar.Calendars\n        Test.Calendar.CalendarsRef\n        Test.Calendar.ClipDates\n        Test.Calendar.ClipDatesRef\n        Test.Calendar.ConvertBack\n        Test.Calendar.Duration\n        Test.Calendar.Easter\n        Test.Calendar.EasterRef\n        Test.Calendar.DayPeriod\n        Test.Calendar.LongWeekYears\n        Test.Calendar.LongWeekYearsRef\n        Test.Calendar.MonthDay\n        Test.Calendar.MonthDayRef\n        Test.Calendar.MonthOfYear\n        Test.Calendar.Valid\n        Test.Calendar.Week\n        Test.Calendar.Year\n        Test.Clock.Conversion\n        Test.Clock.Resolution\n        Test.Clock.TAI\n        Test.Format.Compile\n        Test.Format.Format\n        Test.Format.ParseTime\n        Test.Format.ISO8601\n        Test.LocalTime.CalendarDiffTime\n        Test.LocalTime.Time\n        Test.LocalTime.TimeOfDay\n        Test.LocalTime.TimeRef\n\ntest-suite test-unix\n    if os(windows)\n        buildable: False\n    type: exitcode-stdio-1.0\n    hs-source-dirs: test/unix\n    default-language: Haskell2010\n    default-extensions:\n        Rank2Types\n        DeriveDataTypeable\n        StandaloneDeriving\n        ExistentialQuantification\n        MultiParamTypeClasses\n        FlexibleInstances\n        UndecidableInstances\n        ScopedTypeVariables\n    ghc-options: -Wall -fwarn-tabs\n    c-sources: test/unix/Test/Format/FormatStuff.c\n    build-depends:\n        base,\n        deepseq,\n        time,\n        random,\n        QuickCheck,\n        tasty,\n        tasty-hunit,\n        tasty-quickcheck\n    main-is: Main.hs\n    other-modules:\n        Test.TestUtil\n        Test.Format.Format\n        Test.LocalTime.TimeZone\n\nbenchmark time-bench\n    type: exitcode-stdio-1.0\n    hs-source-dirs: benchmark\n    default-language: Haskell2010\n    ghc-options: -Wall -fwarn-tabs\n    build-depends:\n        base,\n        deepseq,\n        time,\n        criterion\n    main-is: Main.hs\n";
    }