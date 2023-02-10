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
    flags = { native-dns = true; lukko = true; };
    package = {
      specVersion = "2.2";
      identifier = { name = "cabal-install"; version = "3.8.1.0"; };
      license = "BSD-3-Clause";
      copyright = "2003-2022, Cabal Development Team";
      maintainer = "Cabal Development Team <cabal-devel@haskell.org>";
      author = "Cabal Development Team (see AUTHORS file)";
      homepage = "http://www.haskell.org/cabal/";
      url = "";
      synopsis = "The command-line interface for Cabal and Hackage.";
      description = "The \\'cabal\\' command-line program simplifies the process of managing\nHaskell software by automating the fetching, configuration, compilation\nand installation of Haskell libraries and programs.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
          (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
          (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base16-bytestring" or (errorHandler.buildDepError "base16-bytestring"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptohash-sha256" or (errorHandler.buildDepError "cryptohash-sha256"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."echo" or (errorHandler.buildDepError "echo"))
          (hsPkgs."edit-distance" or (errorHandler.buildDepError "edit-distance"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
          (hsPkgs."HTTP" or (errorHandler.buildDepError "HTTP"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."process" or (errorHandler.buildDepError "process"))
          (hsPkgs."random" or (errorHandler.buildDepError "random"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."hackage-security" or (errorHandler.buildDepError "hackage-security"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."regex-base" or (errorHandler.buildDepError "regex-base"))
          (hsPkgs."regex-posix" or (errorHandler.buildDepError "regex-posix"))
          (hsPkgs."safe-exceptions" or (errorHandler.buildDepError "safe-exceptions"))
          ] ++ (pkgs.lib).optionals (flags.native-dns) (if system.isWindows
          then [ (hsPkgs."windns" or (errorHandler.buildDepError "windns")) ]
          else [
            (hsPkgs."resolv" or (errorHandler.buildDepError "resolv"))
            ])) ++ (if system.isWindows
          then [
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            ]
          else [
            (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
            ])) ++ (pkgs.lib).optional (flags.lukko) (hsPkgs."lukko" or (errorHandler.buildDepError "lukko"));
        buildable = true;
        };
      exes = {
        "cabal" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ];
          libs = (pkgs.lib).optional (system.isAix) (pkgs."bsd" or (errorHandler.sysDepError "bsd"));
          buildable = true;
          };
        };
      tests = {
        "unit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."array" or (errorHandler.buildDepError "array"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."Cabal-tree-diff" or (errorHandler.buildDepError "Cabal-tree-diff"))
            (hsPkgs."Cabal-QuickCheck" or (errorHandler.buildDepError "Cabal-QuickCheck"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tar" or (errorHandler.buildDepError "tar"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-golden" or (errorHandler.buildDepError "tasty-golden"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tree-diff" or (errorHandler.buildDepError "tree-diff"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            ];
          buildable = true;
          };
        "mem-use-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            ];
          buildable = true;
          };
        "integration-tests2" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            ];
          buildable = true;
          };
        "long-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."Cabal" or (errorHandler.buildDepError "Cabal"))
            (hsPkgs."Cabal-syntax" or (errorHandler.buildDepError "Cabal-syntax"))
            (hsPkgs."cabal-install-solver" or (errorHandler.buildDepError "cabal-install-solver"))
            (hsPkgs."Cabal-QuickCheck" or (errorHandler.buildDepError "Cabal-QuickCheck"))
            (hsPkgs."Cabal-described" or (errorHandler.buildDepError "Cabal-described"))
            (hsPkgs."cabal-install" or (errorHandler.buildDepError "cabal-install"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."hashable" or (errorHandler.buildDepError "hashable"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."network-uri" or (errorHandler.buildDepError "network-uri"))
            (hsPkgs."random" or (errorHandler.buildDepError "random"))
            (hsPkgs."tagged" or (errorHandler.buildDepError "tagged"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-expected-failure" or (errorHandler.buildDepError "tasty-expected-failure"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."tasty-quickcheck" or (errorHandler.buildDepError "tasty-quickcheck"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."pretty-show" or (errorHandler.buildDepError "pretty-show"))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cabal-install-3.8.1.0.tar.gz";
      sha256 = "61ce436f2e14e12bf07ea1c81402362f46275014cd841a76566f0766d0ea67e6";
      });
    }) // {
    package-description-override = "Cabal-Version:      2.2\r\n\r\nName:               cabal-install\r\nVersion:            3.8.1.0\r\nx-revision: 2\r\nSynopsis:           The command-line interface for Cabal and Hackage.\r\nDescription:\r\n    The \\'cabal\\' command-line program simplifies the process of managing\r\n    Haskell software by automating the fetching, configuration, compilation\r\n    and installation of Haskell libraries and programs.\r\nhomepage:           http://www.haskell.org/cabal/\r\nbug-reports:        https://github.com/haskell/cabal/issues\r\nLicense:            BSD-3-Clause\r\nLicense-File:       LICENSE\r\nAuthor:             Cabal Development Team (see AUTHORS file)\r\nMaintainer:         Cabal Development Team <cabal-devel@haskell.org>\r\nCopyright:          2003-2022, Cabal Development Team\r\nCategory:           Distribution\r\nBuild-type:         Simple\r\nExtra-Source-Files:\r\n  README.md\r\n  bash-completion/cabal\r\n  changelog\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: https://github.com/haskell/cabal/\r\n  subdir:   cabal-install\r\n\r\nFlag native-dns\r\n  description:\r\n    Enable use of the [resolv](https://hackage.haskell.org/package/resolv)\r\n    & [windns](https://hackage.haskell.org/package/windns) packages for performing DNS lookups\r\n  default:      True\r\n  manual:       True\r\n\r\nFlag lukko\r\n  description:  Use @lukko@ for file-locking\r\n  default:      True\r\n  manual:       True\r\n\r\ncommon warnings\r\n    ghc-options: -Wall -Wcompat -Wnoncanonical-monad-instances -Wincomplete-uni-patterns -Wincomplete-record-updates\r\n    if impl(ghc < 8.8)\r\n      ghc-options: -Wnoncanonical-monadfail-instances\r\n    if impl(ghc >=8.10)\r\n      ghc-options: -Wunused-packages\r\n\r\ncommon base-dep\r\n    build-depends: base >=4.10 && <4.18\r\n\r\ncommon cabal-dep\r\n    build-depends: Cabal ^>=3.8\r\n\r\ncommon cabal-syntax-dep\r\n    build-depends: Cabal-syntax ^>=3.8\r\n\r\ncommon cabal-install-solver-dep\r\n    build-depends: cabal-install-solver ^>=3.8\r\n\r\nlibrary\r\n    import: warnings, base-dep, cabal-dep, cabal-syntax-dep, cabal-install-solver-dep\r\n    default-language: Haskell2010\r\n\r\n    hs-source-dirs:   src\r\n    exposed-modules:\r\n        -- this modules are moved from Cabal\r\n        -- they are needed for as long until cabal-install moves to parsec parser\r\n        Distribution.Deprecated.ParseUtils\r\n        Distribution.Deprecated.ReadP\r\n        Distribution.Deprecated.ViewAsFieldDescr\r\n\r\n        Distribution.Client.BuildReports.Anonymous\r\n        Distribution.Client.BuildReports.Lens\r\n        Distribution.Client.BuildReports.Storage\r\n        Distribution.Client.BuildReports.Types\r\n        Distribution.Client.BuildReports.Upload\r\n        Distribution.Client.Check\r\n        Distribution.Client.CmdBench\r\n        Distribution.Client.CmdBuild\r\n        Distribution.Client.CmdClean\r\n        Distribution.Client.CmdConfigure\r\n        Distribution.Client.CmdErrorMessages\r\n        Distribution.Client.CmdExec\r\n        Distribution.Client.CmdFreeze\r\n        Distribution.Client.CmdHaddock\r\n        Distribution.Client.CmdInstall\r\n        Distribution.Client.CmdInstall.ClientInstallFlags\r\n        Distribution.Client.CmdInstall.ClientInstallTargetSelector\r\n        Distribution.Client.CmdLegacy\r\n        Distribution.Client.CmdListBin\r\n        Distribution.Client.CmdOutdated\r\n        Distribution.Client.CmdRepl\r\n        Distribution.Client.CmdRun\r\n        Distribution.Client.CmdSdist\r\n        Distribution.Client.CmdTest\r\n        Distribution.Client.CmdUpdate\r\n        Distribution.Client.Compat.Directory\r\n        Distribution.Client.Compat.ExecutablePath\r\n        Distribution.Client.Compat.Orphans\r\n        Distribution.Client.Compat.Prelude\r\n        Distribution.Client.Compat.Process\r\n        Distribution.Client.Compat.Semaphore\r\n        Distribution.Client.Config\r\n        Distribution.Client.Configure\r\n        Distribution.Client.Dependency\r\n        Distribution.Client.Dependency.Types\r\n        Distribution.Client.DistDirLayout\r\n        Distribution.Client.Fetch\r\n        Distribution.Client.FetchUtils\r\n        Distribution.Client.FileMonitor\r\n        Distribution.Client.Freeze\r\n        Distribution.Client.GZipUtils\r\n        Distribution.Client.GenBounds\r\n        Distribution.Client.Get\r\n        Distribution.Client.Glob\r\n        Distribution.Client.GlobalFlags\r\n        Distribution.Client.Haddock\r\n        Distribution.Client.HashValue\r\n        Distribution.Client.HttpUtils\r\n        Distribution.Client.IndexUtils\r\n        Distribution.Client.IndexUtils.ActiveRepos\r\n        Distribution.Client.IndexUtils.IndexState\r\n        Distribution.Client.IndexUtils.Timestamp\r\n        Distribution.Client.Init\r\n        Distribution.Client.Init.Defaults\r\n        Distribution.Client.Init.FileCreators\r\n        Distribution.Client.Init.FlagExtractors\r\n        Distribution.Client.Init.Format\r\n        Distribution.Client.Init.Interactive.Command\r\n        Distribution.Client.Init.NonInteractive.Command\r\n        Distribution.Client.Init.NonInteractive.Heuristics\r\n        Distribution.Client.Init.Licenses\r\n        Distribution.Client.Init.Prompt\r\n        Distribution.Client.Init.Simple\r\n        Distribution.Client.Init.Types\r\n        Distribution.Client.Init.Utils\r\n        Distribution.Client.Install\r\n        Distribution.Client.InstallPlan\r\n        Distribution.Client.InstallSymlink\r\n        Distribution.Client.JobControl\r\n        Distribution.Client.List\r\n        Distribution.Client.Manpage\r\n        Distribution.Client.ManpageFlags\r\n        Distribution.Client.Nix\r\n        Distribution.Client.NixStyleOptions\r\n        Distribution.Client.PackageHash\r\n        Distribution.Client.ParseUtils\r\n        Distribution.Client.ProjectBuilding\r\n        Distribution.Client.ProjectBuilding.Types\r\n        Distribution.Client.ProjectConfig\r\n        Distribution.Client.ProjectConfig.Legacy\r\n        Distribution.Client.ProjectConfig.Types\r\n        Distribution.Client.ProjectFlags\r\n        Distribution.Client.ProjectOrchestration\r\n        Distribution.Client.ProjectPlanOutput\r\n        Distribution.Client.ProjectPlanning\r\n        Distribution.Client.ProjectPlanning.Types\r\n        Distribution.Client.RebuildMonad\r\n        Distribution.Client.Reconfigure\r\n        Distribution.Client.Run\r\n        Distribution.Client.Sandbox\r\n        Distribution.Client.Sandbox.PackageEnvironment\r\n        Distribution.Client.SavedFlags\r\n        Distribution.Client.ScriptUtils\r\n        Distribution.Client.Security.DNS\r\n        Distribution.Client.Security.HTTP\r\n        Distribution.Client.Setup\r\n        Distribution.Client.SetupWrapper\r\n        Distribution.Client.SolverInstallPlan\r\n        Distribution.Client.SourceFiles\r\n        Distribution.Client.SrcDist\r\n        Distribution.Client.Store\r\n        Distribution.Client.Tar\r\n        Distribution.Client.TargetProblem\r\n        Distribution.Client.TargetSelector\r\n        Distribution.Client.Targets\r\n        Distribution.Client.Types\r\n        Distribution.Client.Types.AllowNewer\r\n        Distribution.Client.Types.BuildResults\r\n        Distribution.Client.Types.ConfiguredId\r\n        Distribution.Client.Types.ConfiguredPackage\r\n        Distribution.Client.Types.Credentials\r\n        Distribution.Client.Types.InstallMethod\r\n        Distribution.Client.Types.OverwritePolicy\r\n        Distribution.Client.Types.PackageLocation\r\n        Distribution.Client.Types.PackageSpecifier\r\n        Distribution.Client.Types.ReadyPackage\r\n        Distribution.Client.Types.Repo\r\n        Distribution.Client.Types.RepoName\r\n        Distribution.Client.Types.SourcePackageDb\r\n        Distribution.Client.Types.SourceRepo\r\n        Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy\r\n        Distribution.Client.Upload\r\n        Distribution.Client.Utils\r\n        Distribution.Client.Utils.Json\r\n        Distribution.Client.Utils.Parsec\r\n        Distribution.Client.VCS\r\n        Distribution.Client.Version\r\n        Distribution.Client.Win32SelfUpgrade\r\n\r\n    build-depends:\r\n        async      >= 2.0      && < 2.3,\r\n        array      >= 0.4      && < 0.6,\r\n        base16-bytestring >= 0.1.1 && < 1.1.0.0,\r\n        binary     >= 0.7.3    && < 0.9,\r\n        bytestring >= 0.10.6.0 && < 0.12,\r\n        containers >= 0.5.6.2  && < 0.7,\r\n        cryptohash-sha256 >= 0.11 && < 0.12,\r\n        directory  >= 1.2.2.0  && < 1.4,\r\n        echo       >= 0.1.3    && < 0.2,\r\n        edit-distance >= 0.2.2 && < 0.3,\r\n        exceptions >= 0.10.4   && < 0.11,\r\n        filepath   >= 1.4.0.0  && < 1.5,\r\n        hashable   >= 1.0      && < 1.5,\r\n        HTTP       >= 4000.1.5 && < 4000.5,\r\n        mtl        >= 2.0      && < 2.3,\r\n        network-uri >= 2.6.0.2 && < 2.7,\r\n        pretty     >= 1.1      && < 1.2,\r\n        process    >= 1.2.3.0  && < 1.7,\r\n        random     >= 1.2      && < 1.3,\r\n        stm        >= 2.0      && < 2.6,\r\n        tar        >= 0.5.0.3  && < 0.6,\r\n        time       >= 1.5.0.1  && < 1.13,\r\n        zlib       >= 0.5.3    && < 0.7,\r\n        hackage-security >= 0.6.2.0 && < 0.7,\r\n        text       >= 1.2.3    && < 1.3 || >= 2.0 && < 2.1,\r\n        parsec     >= 3.1.13.0 && < 3.2,\r\n        regex-base  >= 0.94.0.0 && <0.95,\r\n        regex-posix >= 0.96.0.0 && <0.97,\r\n        safe-exceptions >= 0.1.7.0 && < 0.2\r\n\r\n    if flag(native-dns)\r\n      if os(windows)\r\n        build-depends: windns      >= 0.1.0 && < 0.2\r\n      else\r\n        build-depends: resolv      >= 0.1.1 && < 0.2\r\n\r\n    if os(windows)\r\n      -- newer directory for symlinks\r\n      build-depends: Win32 >= 2.8 && < 3, directory >=1.3.1.0\r\n    else\r\n      build-depends: unix >= 2.5 && < 2.9\r\n\r\n    if flag(lukko)\r\n      build-depends: lukko >= 0.1 && <0.2\r\n\r\n\r\nexecutable cabal\r\n    import: warnings, base-dep, cabal-dep, cabal-syntax-dep\r\n    main-is: Main.hs\r\n    hs-source-dirs: main\r\n    default-language: Haskell2010\r\n\r\n    ghc-options: -rtsopts -threaded\r\n\r\n    -- On AIX, some legacy BSD operations such as flock(2) are provided by libbsd.a\r\n    if os(aix)\r\n        extra-libraries: bsd\r\n\r\n    build-depends:\r\n        cabal-install,\r\n        directory,\r\n        filepath\r\n\r\n-- Small, fast running tests.\r\n--\r\ntest-suite unit-tests\r\n    import: warnings, base-dep, cabal-dep, cabal-syntax-dep, cabal-install-solver-dep\r\n    default-language: Haskell2010\r\n    ghc-options: -rtsopts -threaded\r\n\r\n    type: exitcode-stdio-1.0\r\n    main-is: UnitTests.hs\r\n    hs-source-dirs: tests\r\n    other-modules:\r\n      UnitTests.Distribution.Client.ArbitraryInstances\r\n      UnitTests.Distribution.Client.BuildReport\r\n      UnitTests.Distribution.Client.Configure\r\n      UnitTests.Distribution.Client.FetchUtils\r\n      UnitTests.Distribution.Client.Get\r\n      UnitTests.Distribution.Client.Glob\r\n      UnitTests.Distribution.Client.GZipUtils\r\n      UnitTests.Distribution.Client.IndexUtils\r\n      UnitTests.Distribution.Client.IndexUtils.Timestamp\r\n      UnitTests.Distribution.Client.Init\r\n      UnitTests.Distribution.Client.Init.Golden\r\n      UnitTests.Distribution.Client.Init.Interactive\r\n      UnitTests.Distribution.Client.Init.NonInteractive\r\n      UnitTests.Distribution.Client.Init.Simple\r\n      UnitTests.Distribution.Client.Init.Utils\r\n      UnitTests.Distribution.Client.Init.FileCreators\r\n      UnitTests.Distribution.Client.InstallPlan\r\n      UnitTests.Distribution.Client.JobControl\r\n      UnitTests.Distribution.Client.ProjectConfig\r\n      UnitTests.Distribution.Client.ProjectPlanning\r\n      UnitTests.Distribution.Client.Store\r\n      UnitTests.Distribution.Client.Tar\r\n      UnitTests.Distribution.Client.Targets\r\n      UnitTests.Distribution.Client.TreeDiffInstances\r\n      UnitTests.Distribution.Client.UserConfig\r\n      UnitTests.Distribution.Solver.Modular.Builder\r\n      UnitTests.Distribution.Solver.Modular.RetryLog\r\n      UnitTests.Distribution.Solver.Modular.Solver\r\n      UnitTests.Distribution.Solver.Modular.DSL\r\n      UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils\r\n      UnitTests.Distribution.Solver.Modular.WeightedPSQ\r\n      UnitTests.Distribution.Solver.Types.OptionalStanza\r\n      UnitTests.Options\r\n      UnitTests.TempTestDir\r\n\r\n    build-depends:\r\n          array,\r\n          bytestring,\r\n          cabal-install,\r\n          Cabal-tree-diff,\r\n          Cabal-QuickCheck,\r\n          containers,\r\n          directory,\r\n          filepath,\r\n          mtl,\r\n          network-uri >= 2.6.2.0 && <2.7,\r\n          random,\r\n          tar,\r\n          time,\r\n          zlib,\r\n          tasty >= 1.2.3 && <1.5,\r\n          tasty-golden >=2.3.1.1 && <2.4,\r\n          tasty-quickcheck,\r\n          tasty-hunit >= 0.10,\r\n          tree-diff,\r\n          QuickCheck >= 2.14 && <2.15\r\n\r\n\r\n-- Tests to run with a limited stack and heap size\r\n-- The test suite name must be keep short cause a longer one\r\n-- could make the build generating paths which exceeds the windows\r\n-- max path limit (still a problem for some ghc versions)\r\ntest-suite mem-use-tests\r\n  import: warnings, base-dep, cabal-dep, cabal-syntax-dep, cabal-install-solver-dep\r\n  type: exitcode-stdio-1.0\r\n  main-is: MemoryUsageTests.hs\r\n  hs-source-dirs: tests\r\n  default-language: Haskell2010\r\n\r\n  ghc-options: -threaded -rtsopts \"-with-rtsopts=-M16M -K1K\"\r\n\r\n  other-modules:\r\n    UnitTests.Distribution.Solver.Modular.DSL\r\n    UnitTests.Distribution.Solver.Modular.DSL.TestCaseUtils\r\n    UnitTests.Distribution.Solver.Modular.MemoryUsage\r\n    UnitTests.Options\r\n\r\n  build-depends:\r\n        cabal-install,\r\n        containers,\r\n        tasty >= 1.2.3 && <1.5,\r\n        tasty-hunit >= 0.10\r\n\r\n\r\n-- Integration tests that use the cabal-install code directly\r\n-- but still build whole projects\r\ntest-suite integration-tests2\r\n  import: warnings, base-dep, cabal-dep, cabal-syntax-dep, cabal-install-solver-dep\r\n  ghc-options: -rtsopts -threaded\r\n  type: exitcode-stdio-1.0\r\n  main-is: IntegrationTests2.hs\r\n  hs-source-dirs: tests\r\n  default-language: Haskell2010\r\n\r\n  build-depends:\r\n        bytestring,\r\n        cabal-install,\r\n        containers,\r\n        directory,\r\n        filepath,\r\n        tasty >= 1.2.3 && <1.5,\r\n        tasty-hunit >= 0.10,\r\n        tagged\r\n\r\ntest-suite long-tests\r\n  import: warnings, base-dep, cabal-dep, cabal-syntax-dep, cabal-install-solver-dep\r\n  ghc-options: -rtsopts -threaded\r\n  type: exitcode-stdio-1.0\r\n  hs-source-dirs: tests\r\n  main-is: LongTests.hs\r\n  default-language: Haskell2010\r\n\r\n  other-modules:\r\n    UnitTests.Distribution.Client.ArbitraryInstances\r\n    UnitTests.Distribution.Client.Described\r\n    UnitTests.Distribution.Client.DescribedInstances\r\n    UnitTests.Distribution.Client.FileMonitor\r\n    UnitTests.Distribution.Client.VCS\r\n    UnitTests.Distribution.Solver.Modular.DSL\r\n    UnitTests.Distribution.Solver.Modular.QuickCheck\r\n    UnitTests.Distribution.Solver.Modular.QuickCheck.Utils\r\n    UnitTests.Options\r\n    UnitTests.TempTestDir\r\n\r\n  build-depends:\r\n        Cabal-QuickCheck,\r\n        Cabal-described,\r\n        cabal-install,\r\n        containers,\r\n        directory,\r\n        filepath,\r\n        hashable,\r\n        mtl,\r\n        network-uri >= 2.6.2.0 && <2.7,\r\n        random,\r\n        tagged,\r\n        tasty >= 1.2.3 && <1.5,\r\n        tasty-expected-failure,\r\n        tasty-hunit >= 0.10,\r\n        tasty-quickcheck,\r\n        QuickCheck >= 2.14 && <2.15,\r\n        pretty-show >= 1.6.15\r\n";
    }