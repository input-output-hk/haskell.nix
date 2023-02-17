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
    flags = { portable = false; cloud = false; embed-files = false; };
    package = {
      specVersion = "1.18";
      identifier = { name = "shake"; version = "0.19.7"; };
      license = "BSD-3-Clause";
      copyright = "Neil Mitchell 2011-2022";
      maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
      author = "Neil Mitchell <ndmitchell@gmail.com>";
      homepage = "https://shakebuild.com";
      url = "";
      synopsis = "Build system library, like Make, but more accurate dependencies.";
      description = "Shake is a Haskell library for writing build systems - designed as a\nreplacement for @make@. See \"Development.Shake\" for an introduction,\nincluding an example. The homepage contains links to a user\nmanual, an academic paper and further information:\n<https://shakebuild.com>\n\nTo use Shake the user writes a Haskell program\nthat imports \"Development.Shake\", defines some build rules, and calls\nthe 'Development.Shake.shakeArgs' function. Thanks to do notation and infix\noperators, a simple Shake build system\nis not too dissimilar from a simple Makefile. However, as build systems\nget more complex, Shake is able to take advantage of the excellent\nabstraction facilities offered by Haskell and easily support much larger\nprojects. The Shake library provides all the standard features available in other\nbuild systems, including automatic parallelism and minimal rebuilds.\nShake also provides more accurate dependency tracking, including seamless\nsupport for generated files, and dependencies on system information\n(e.g. compiler version).";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ((([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."filepattern" or (errorHandler.buildDepError "filepattern"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."heaps" or (errorHandler.buildDepError "heaps"))
          (hsPkgs."js-dgtable" or (errorHandler.buildDepError "js-dgtable"))
          (hsPkgs."js-flot" or (errorHandler.buildDepError "js-flot"))
          (hsPkgs."js-jquery" or (errorHandler.buildDepError "js-jquery"))
          (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          ] ++ (pkgs.lib).optionals (flags.embed-files) [
          (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
          (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
          ]) ++ (pkgs.lib).optionals (!flags.portable) ((pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix")))) ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optionals (flags.cloud) [
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          ];
        buildable = true;
        };
      exes = {
        "shake" = {
          depends = (((([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."filepattern" or (errorHandler.buildDepError "filepattern"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."heaps" or (errorHandler.buildDepError "heaps"))
            (hsPkgs."js-dgtable" or (errorHandler.buildDepError "js-dgtable"))
            (hsPkgs."js-flot" or (errorHandler.buildDepError "js-flot"))
            (hsPkgs."js-jquery" or (errorHandler.buildDepError "js-jquery"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ] ++ (pkgs.lib).optionals (flags.embed-files) [
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ]) ++ (pkgs.lib).optionals (!flags.portable) ((pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix")))) ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optionals (flags.cloud) [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      tests = {
        "shake-test" = {
          depends = (((([
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."extra" or (errorHandler.buildDepError "extra"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."filepattern" or (errorHandler.buildDepError "filepattern"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."heaps" or (errorHandler.buildDepError "heaps"))
            (hsPkgs."js-dgtable" or (errorHandler.buildDepError "js-dgtable"))
            (hsPkgs."js-flot" or (errorHandler.buildDepError "js-flot"))
            (hsPkgs."js-jquery" or (errorHandler.buildDepError "js-jquery"))
            (hsPkgs."primitive" or (errorHandler.buildDepError "primitive"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
            (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
            ] ++ (pkgs.lib).optionals (flags.embed-files) [
            (hsPkgs."file-embed" or (errorHandler.buildDepError "file-embed"))
            (hsPkgs."template-haskell" or (errorHandler.buildDepError "template-haskell"))
            ]) ++ (pkgs.lib).optionals (!flags.portable) ((pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix")))) ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"))) ++ (pkgs.lib).optionals (flags.cloud) [
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            ]) ++ (pkgs.lib).optional (compiler.isGhc && (compiler.version).lt "8.0") (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"));
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/shake-0.19.7.tar.gz";
      sha256 = "352a56af12f70b50d564dcb61131555577281957ee196f1702a3723c0a3699d1";
      });
    }) // {
    package-description-override = "cabal-version:      1.18\nbuild-type:         Simple\nname:               shake\nversion:            0.19.7\nlicense:            BSD3\nlicense-file:       LICENSE\ncategory:           Development, Shake\nauthor:             Neil Mitchell <ndmitchell@gmail.com>\nmaintainer:         Neil Mitchell <ndmitchell@gmail.com>\ncopyright:          Neil Mitchell 2011-2022\nsynopsis:           Build system library, like Make, but more accurate dependencies.\ndescription:\n    Shake is a Haskell library for writing build systems - designed as a\n    replacement for @make@. See \"Development.Shake\" for an introduction,\n    including an example. The homepage contains links to a user\n    manual, an academic paper and further information:\n    <https://shakebuild.com>\n    .\n    To use Shake the user writes a Haskell program\n    that imports \"Development.Shake\", defines some build rules, and calls\n    the 'Development.Shake.shakeArgs' function. Thanks to do notation and infix\n    operators, a simple Shake build system\n    is not too dissimilar from a simple Makefile. However, as build systems\n    get more complex, Shake is able to take advantage of the excellent\n    abstraction facilities offered by Haskell and easily support much larger\n    projects. The Shake library provides all the standard features available in other\n    build systems, including automatic parallelism and minimal rebuilds.\n    Shake also provides more accurate dependency tracking, including seamless\n    support for generated files, and dependencies on system information\n    (e.g. compiler version).\nhomepage:           https://shakebuild.com\nbug-reports:        https://github.com/ndmitchell/shake/issues\ntested-with:        GHC==9.0, GHC==8.10, GHC==8.8, GHC==8.6\nextra-doc-files:\n    CHANGES.txt\n    README.md\n    docs/Manual.md\n    docs/shake-progress.png\nextra-source-files:\n    src/Paths.hs\n    src/Test/C/constants.c\n    src/Test/C/constants.h\n    src/Test/C/main.c\n    src/Test/Ninja/*.ninja\n    src/Test/Ninja/*.output\n    src/Test/Ninja/subdir/*.ninja\n    src/Test/Progress/*.prog\n    src/Test/Tar/list.txt\n    src/Test/Tup/hello.c\n    src/Test/Tup/newmath/root.cfg\n    src/Test/Tup/newmath/square.c\n    src/Test/Tup/newmath/square.h\n    src/Test/Tup/root.cfg\ndata-files:\n    docs/manual/build.bat\n    docs/manual/Shakefile.hs\n    docs/manual/build.sh\n    docs/manual/constants.c\n    docs/manual/constants.h\n    docs/manual/main.c\n    html/profile.html\n    html/progress.html\n    html/shake.js\n\nsource-repository head\n    type:     git\n    location: https://github.com/ndmitchell/shake.git\n\nflag portable\n    default: False\n    manual: True\n    description: Obtain FileTime using portable functions\n\nflag cloud\n    default: False\n    manual: True\n    description: Enable cloud build features\n\nflag embed-files\n    default: False\n    manual: True\n    description: Embed data files into the shake library\n\nlibrary\n    default-language: Haskell2010\n    hs-source-dirs:   src\n    build-depends:\n        base >= 4.9,\n        binary,\n        bytestring,\n        deepseq >= 1.1,\n        directory >= 1.2.7.0,\n        extra >= 1.6.19,\n        filepath,\n        filepattern,\n        hashable >= 1.1.2.3,\n        heaps >= 0.3.6.1,\n        js-dgtable,\n        js-flot,\n        js-jquery,\n        primitive,\n        process >= 1.1,\n        random,\n        time,\n        transformers >= 0.2,\n        unordered-containers >= 0.2.7,\n        utf8-string >= 0.3\n\n    if flag(embed-files)\n        cpp-options: -DFILE_EMBED\n        build-depends:\n            file-embed >= 0.0.11,\n            template-haskell\n\n    if flag(portable)\n        cpp-options: -DPORTABLE\n    else\n        if !os(windows)\n            build-depends: unix >= 2.5.1\n    if !os(windows)\n        build-depends: unix\n\n    if flag(cloud)\n        cpp-options: -DNETWORK\n        build-depends: network, network-uri\n\n    exposed-modules:\n        Development.Shake\n        Development.Shake.Classes\n        Development.Shake.Command\n        Development.Shake.Config\n        Development.Shake.Database\n        Development.Shake.FilePath\n        Development.Shake.Forward\n        Development.Shake.Rule\n        Development.Shake.Util\n\n    other-modules:\n        Development.Ninja.Env\n        Development.Ninja.Lexer\n        Development.Ninja.Parse\n        Development.Ninja.Type\n        Development.Shake.Internal.Args\n        Development.Shake.Internal.CmdOption\n        Development.Shake.Internal.CompactUI\n        Development.Shake.Internal.Core.Action\n        Development.Shake.Internal.Core.Build\n        Development.Shake.Internal.Core.Database\n        Development.Shake.Internal.History.Shared\n        Development.Shake.Internal.History.Symlink\n        Development.Shake.Internal.History.Bloom\n        Development.Shake.Internal.History.Cloud\n        Development.Shake.Internal.History.Network\n        Development.Shake.Internal.History.Server\n        Development.Shake.Internal.History.Serialise\n        Development.Shake.Internal.History.Types\n        Development.Shake.Internal.Core.Monad\n        Development.Shake.Internal.Core.Pool\n        Development.Shake.Internal.Core.Rules\n        Development.Shake.Internal.Core.Run\n        Development.Shake.Internal.Core.Storage\n        Development.Shake.Internal.Core.Types\n        Development.Shake.Internal.Demo\n        Development.Shake.Internal.Derived\n        Development.Shake.Internal.Errors\n        Development.Shake.Internal.FileInfo\n        Development.Shake.Internal.FileName\n        Development.Shake.Internal.FilePattern\n        Development.Shake.Internal.Options\n        Development.Shake.Internal.Paths\n        Development.Shake.Internal.Profile\n        Development.Shake.Internal.Progress\n        Development.Shake.Internal.Resource\n        Development.Shake.Internal.Rules.Default\n        Development.Shake.Internal.Rules.Directory\n        Development.Shake.Internal.Rules.File\n        Development.Shake.Internal.Rules.Files\n        Development.Shake.Internal.Rules.Oracle\n        Development.Shake.Internal.Rules.OrderOnly\n        Development.Shake.Internal.Rules.Rerun\n        Development.Shake.Internal.Value\n        General.Bilist\n        General.Binary\n        General.Chunks\n        General.Cleanup\n        General.Fence\n        General.EscCodes\n        General.Extra\n        General.FileLock\n        General.GetOpt\n        General.Ids\n        General.Intern\n        General.ListBuilder\n        General.Makefile\n        General.Pool\n        General.Process\n        General.Template\n        General.Thread\n        General.Timing\n        General.TypeMap\n        General.Wait\n        Paths_shake\n\n\nexecutable shake\n    default-language: Haskell2010\n    hs-source-dirs:   src\n    ghc-options: -main-is Run.main -rtsopts -threaded \"-with-rtsopts=-I0 -qg\"\n    main-is: Run.hs\n    build-depends:\n        base == 4.*,\n        binary,\n        bytestring,\n        deepseq >= 1.1,\n        directory,\n        extra >= 1.6.19,\n        filepath,\n        filepattern,\n        hashable >= 1.1.2.3,\n        heaps >= 0.3.6.1,\n        js-dgtable,\n        js-flot,\n        js-jquery,\n        primitive,\n        process >= 1.1,\n        random,\n        time,\n        transformers >= 0.2,\n        unordered-containers >= 0.2.7,\n        utf8-string >= 0.3\n\n    if flag(embed-files)\n        cpp-options: -DFILE_EMBED\n        build-depends:\n            file-embed >= 0.0.11,\n            template-haskell\n\n    if flag(portable)\n        cpp-options: -DPORTABLE\n    else\n        if !os(windows)\n            build-depends: unix >= 2.5.1\n    if !os(windows)\n        build-depends: unix\n\n    if flag(cloud)\n        cpp-options: -DNETWORK\n        build-depends: network, network-uri\n\n    if impl(ghc < 8.0)\n        build-depends: semigroups >= 0.18\n\n    other-modules:\n        Development.Ninja.All\n        Development.Ninja.Env\n        Development.Ninja.Lexer\n        Development.Ninja.Parse\n        Development.Ninja.Type\n        Development.Shake\n        Development.Shake.Classes\n        Development.Shake.Command\n        Development.Shake.Database\n        Development.Shake.FilePath\n        Development.Shake.Internal.Args\n        Development.Shake.Internal.CmdOption\n        Development.Shake.Internal.CompactUI\n        Development.Shake.Internal.Core.Action\n        Development.Shake.Internal.Core.Build\n        Development.Shake.Internal.Core.Database\n        Development.Shake.Internal.History.Shared\n        Development.Shake.Internal.History.Symlink\n        Development.Shake.Internal.History.Bloom\n        Development.Shake.Internal.History.Cloud\n        Development.Shake.Internal.History.Network\n        Development.Shake.Internal.History.Server\n        Development.Shake.Internal.History.Serialise\n        Development.Shake.Internal.History.Types\n        Development.Shake.Internal.Core.Monad\n        Development.Shake.Internal.Core.Pool\n        Development.Shake.Internal.Core.Rules\n        Development.Shake.Internal.Core.Run\n        Development.Shake.Internal.Core.Storage\n        Development.Shake.Internal.Core.Types\n        Development.Shake.Internal.Demo\n        Development.Shake.Internal.Derived\n        Development.Shake.Internal.Errors\n        Development.Shake.Internal.FileInfo\n        Development.Shake.Internal.FileName\n        Development.Shake.Internal.FilePattern\n        Development.Shake.Internal.Options\n        Development.Shake.Internal.Paths\n        Development.Shake.Internal.Profile\n        Development.Shake.Internal.Progress\n        Development.Shake.Internal.Resource\n        Development.Shake.Internal.Rules.Default\n        Development.Shake.Internal.Rules.Directory\n        Development.Shake.Internal.Rules.File\n        Development.Shake.Internal.Rules.Files\n        Development.Shake.Internal.Rules.Oracle\n        Development.Shake.Internal.Rules.OrderOnly\n        Development.Shake.Internal.Rules.Rerun\n        Development.Shake.Internal.Value\n        General.Bilist\n        General.Binary\n        General.Chunks\n        General.Cleanup\n        General.Fence\n        General.EscCodes\n        General.Extra\n        General.FileLock\n        General.GetOpt\n        General.Ids\n        General.Intern\n        General.ListBuilder\n        General.Makefile\n        General.Pool\n        General.Process\n        General.Template\n        General.Thread\n        General.Timing\n        General.TypeMap\n        General.Wait\n        Paths_shake\n\n\ntest-suite shake-test\n    default-language: Haskell2010\n    type: exitcode-stdio-1.0\n    main-is: Test.hs\n    hs-source-dirs: src\n    ghc-options: -main-is Test.main -rtsopts -with-rtsopts=-K1K -threaded\n\n    build-depends:\n        base == 4.*,\n        binary,\n        bytestring,\n        deepseq >= 1.1,\n        directory,\n        extra >= 1.6.19,\n        filepath,\n        filepattern,\n        hashable >= 1.1.2.3,\n        heaps >= 0.3.6.1,\n        js-dgtable,\n        js-flot,\n        js-jquery,\n        primitive,\n        process >= 1.1,\n        QuickCheck >= 2.0,\n        random,\n        time,\n        transformers >= 0.2,\n        unordered-containers >= 0.2.7,\n        utf8-string >= 0.3\n\n    if flag(embed-files)\n        cpp-options: -DFILE_EMBED\n        build-depends:\n            file-embed >= 0.0.11,\n            template-haskell\n\n    if flag(portable)\n        cpp-options: -DPORTABLE\n    else\n        if !os(windows)\n            build-depends: unix >= 2.5.1\n    if !os(windows)\n        build-depends: unix\n\n    if flag(cloud)\n        cpp-options: -DNETWORK\n        build-depends: network, network-uri\n\n    if impl(ghc < 8.0)\n        build-depends: semigroups >= 0.18\n\n    other-modules:\n        Development.Ninja.All\n        Development.Ninja.Env\n        Development.Ninja.Lexer\n        Development.Ninja.Parse\n        Development.Ninja.Type\n        Development.Shake\n        Development.Shake.Classes\n        Development.Shake.Command\n        Development.Shake.Config\n        Development.Shake.Database\n        Development.Shake.FilePath\n        Development.Shake.Forward\n        Development.Shake.Internal.Args\n        Development.Shake.Internal.CmdOption\n        Development.Shake.Internal.CompactUI\n        Development.Shake.Internal.Core.Action\n        Development.Shake.Internal.Core.Build\n        Development.Shake.Internal.Core.Database\n        Development.Shake.Internal.History.Shared\n        Development.Shake.Internal.History.Symlink\n        Development.Shake.Internal.History.Bloom\n        Development.Shake.Internal.History.Cloud\n        Development.Shake.Internal.History.Network\n        Development.Shake.Internal.History.Server\n        Development.Shake.Internal.History.Serialise\n        Development.Shake.Internal.History.Types\n        Development.Shake.Internal.Core.Monad\n        Development.Shake.Internal.Core.Pool\n        Development.Shake.Internal.Core.Rules\n        Development.Shake.Internal.Core.Run\n        Development.Shake.Internal.Core.Storage\n        Development.Shake.Internal.Core.Types\n        Development.Shake.Internal.Demo\n        Development.Shake.Internal.Derived\n        Development.Shake.Internal.Errors\n        Development.Shake.Internal.FileInfo\n        Development.Shake.Internal.FileName\n        Development.Shake.Internal.FilePattern\n        Development.Shake.Internal.Options\n        Development.Shake.Internal.Paths\n        Development.Shake.Internal.Profile\n        Development.Shake.Internal.Progress\n        Development.Shake.Internal.Resource\n        Development.Shake.Internal.Rules.Default\n        Development.Shake.Internal.Rules.Directory\n        Development.Shake.Internal.Rules.File\n        Development.Shake.Internal.Rules.Files\n        Development.Shake.Internal.Rules.Oracle\n        Development.Shake.Internal.Rules.OrderOnly\n        Development.Shake.Internal.Rules.Rerun\n        Development.Shake.Internal.Value\n        Development.Shake.Rule\n        Development.Shake.Util\n        General.Bilist\n        General.Binary\n        General.Chunks\n        General.Cleanup\n        General.Fence\n        General.EscCodes\n        General.Extra\n        General.FileLock\n        General.GetOpt\n        General.Ids\n        General.Intern\n        General.ListBuilder\n        General.Makefile\n        General.Pool\n        General.Process\n        General.Template\n        General.Thread\n        General.Timing\n        General.TypeMap\n        General.Wait\n        Paths_shake\n        Run\n        Test.Basic\n        Test.Batch\n        Test.Benchmark\n        Test.Builtin\n        Test.BuiltinOverride\n        Test.C\n        Test.Cache\n        Test.Cleanup\n        Test.CloseFileHandles\n        Test.Command\n        Test.Config\n        Test.Database\n        Test.Digest\n        Test.Directory\n        Test.Docs\n        Test.Errors\n        Test.Existence\n        Test.FileLock\n        Test.FilePath\n        Test.FilePattern\n        Test.Files\n        Test.Forward\n        Test.History\n        Test.Journal\n        Test.Lint\n        Test.Live\n        Test.Manual\n        Test.Match\n        Test.Monad\n        Test.Ninja\n        Test.Oracle\n        Test.OrderOnly\n        Test.Parallel\n        Test.Pool\n        Test.Progress\n        Test.Random\n        Test.Rebuild\n        Test.Reschedule\n        Test.Resources\n        Test.Self\n        Test.SelfMake\n        Test.Tar\n        Test.Targets\n        Test.Thread\n        Test.Tup\n        Test.Type\n        Test.Unicode\n        Test.Util\n        Test.Verbosity\n        Test.Version\n";
    }