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
      specVersion = "2.2";
      identifier = { name = "Cabal-syntax"; version = "3.10.3.0"; };
      license = "BSD-3-Clause";
      copyright = "2003-2023, Cabal Development Team (see AUTHORS file)";
      maintainer = "cabal-devel@haskell.org";
      author = "Cabal Development Team <cabal-devel@haskell.org>";
      homepage = "http://www.haskell.org/cabal/";
      url = "";
      synopsis = "A library for working with .cabal files";
      description = "This library provides tools for reading and manipulating the .cabal file\nformat.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."deepseq" or (errorHandler.buildDepError "deepseq"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."parsec" or (errorHandler.buildDepError "parsec"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ] ++ (if system.isWindows
          then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/Cabal-syntax-3.10.3.0.tar.gz";
      sha256 = "75d6a0aa9c00990a0d6e7720ac50c6954f6c942fa1be42c8add7f1c025f7e212";
    });
  }) // {
    package-description-override = "cabal-version: 2.2\nname:          Cabal-syntax\nversion:       3.10.3.0\ncopyright:     2003-2023, Cabal Development Team (see AUTHORS file)\nlicense:       BSD-3-Clause\nlicense-file:  LICENSE\nauthor:        Cabal Development Team <cabal-devel@haskell.org>\nmaintainer:    cabal-devel@haskell.org\nhomepage:      http://www.haskell.org/cabal/\nbug-reports:   https://github.com/haskell/cabal/issues\nsynopsis:      A library for working with .cabal files\ndescription:\n    This library provides tools for reading and manipulating the .cabal file\n    format.\ncategory:       Distribution\nbuild-type:     Simple\n\nextra-doc-files:\n  README.md ChangeLog.md\n\nsource-repository head\n  type:     git\n  location: https://github.com/haskell/cabal/\n  subdir:   Cabal-syntax\n\nlibrary\n  default-language: Haskell2010\n  hs-source-dirs: src\n\n  build-depends:\n    array      >= 0.4.0.1  && < 0.6,\n    base       >= 4.9      && < 5,\n    binary     >= 0.7      && < 0.9,\n    bytestring >= 0.10.0.0 && < 0.13,\n    containers >= 0.5.0.0  && < 0.8,\n    deepseq    >= 1.3.0.1  && < 1.6,\n    directory  >= 1.2      && < 1.4,\n    filepath   >= 1.3.0.1  && < 1.6,\n    mtl        >= 2.1      && < 2.4,\n    parsec     >= 3.1.13.0 && < 3.2,\n    pretty     >= 1.1.1    && < 1.2,\n    text       (>= 1.2.3.0 && < 1.3) || (>= 2.0 && < 2.2),\n    time       >= 1.4.0.1  && < 1.13,\n    -- transformers-0.4.0.0 doesn't have record syntax e.g. for Identity\n    -- See also https://github.com/ekmett/transformers-compat/issues/35\n    transformers (>= 0.3      && < 0.4) || (>=0.4.1.0 && <0.7)\n\n  if os(windows)\n    build-depends: Win32 >= 2.3.0.0 && < 2.14\n  else\n    build-depends: unix  >= 2.6.0.0 && < 2.9\n\n  ghc-options: -Wall -fno-ignore-asserts -fwarn-tabs -fwarn-incomplete-uni-patterns -fwarn-incomplete-record-updates\n\n  if impl(ghc >= 8.0)\n    ghc-options: -Wcompat -Wnoncanonical-monad-instances\n\n  if impl(ghc >= 8.0) && impl(ghc < 8.8)\n    ghc-options: -Wnoncanonical-monadfail-instances\n\n  exposed-modules:\n    Distribution.Backpack\n    Distribution.CabalSpecVersion\n    Distribution.Compat.Binary\n    Distribution.Compat.CharParsing\n    Distribution.Compat.DList\n    Distribution.Compat.Exception\n    Distribution.Compat.Graph\n    Distribution.Compat.Lens\n    Distribution.Compat.MonadFail\n    Distribution.Compat.Newtype\n    Distribution.Compat.NonEmptySet\n    Distribution.Compat.Parsing\n    Distribution.Compat.Prelude\n    Distribution.Compat.Semigroup\n    Distribution.Compat.Typeable\n    Distribution.Compiler\n    Distribution.FieldGrammar\n    Distribution.FieldGrammar.Class\n    Distribution.FieldGrammar.FieldDescrs\n    Distribution.FieldGrammar.Newtypes\n    Distribution.FieldGrammar.Parsec\n    Distribution.FieldGrammar.Pretty\n    Distribution.Fields\n    Distribution.Fields.ConfVar\n    Distribution.Fields.Field\n    Distribution.Fields.Lexer\n    Distribution.Fields.LexerMonad\n    Distribution.Fields.ParseResult\n    Distribution.Fields.Parser\n    Distribution.Fields.Pretty\n    Distribution.InstalledPackageInfo\n    Distribution.License\n    Distribution.ModuleName\n    Distribution.Package\n    Distribution.PackageDescription\n    Distribution.PackageDescription.Configuration\n    Distribution.PackageDescription.FieldGrammar\n    Distribution.PackageDescription.Parsec\n    Distribution.PackageDescription.PrettyPrint\n    Distribution.PackageDescription.Quirks\n    Distribution.PackageDescription.Utils\n    Distribution.Parsec\n    Distribution.Parsec.Error\n    Distribution.Parsec.FieldLineStream\n    Distribution.Parsec.Position\n    Distribution.Parsec.Warning\n    Distribution.Pretty\n    Distribution.SPDX\n    Distribution.SPDX.License\n    Distribution.SPDX.LicenseExceptionId\n    Distribution.SPDX.LicenseExpression\n    Distribution.SPDX.LicenseId\n    Distribution.SPDX.LicenseListVersion\n    Distribution.SPDX.LicenseReference\n    Distribution.System\n    Distribution.Text\n    Distribution.Types.AbiDependency\n    Distribution.Types.AbiHash\n    Distribution.Types.Benchmark\n    Distribution.Types.Benchmark.Lens\n    Distribution.Types.BenchmarkInterface\n    Distribution.Types.BenchmarkType\n    Distribution.Types.BuildInfo\n    Distribution.Types.BuildInfo.Lens\n    Distribution.Types.BuildType\n    Distribution.Types.Component\n    Distribution.Types.ComponentId\n    Distribution.Types.ComponentName\n    Distribution.Types.ComponentRequestedSpec\n    Distribution.Types.CondTree\n    Distribution.Types.Condition\n    Distribution.Types.ConfVar\n    Distribution.Types.Dependency\n    Distribution.Types.DependencyMap\n    Distribution.Types.ExeDependency\n    Distribution.Types.Executable\n    Distribution.Types.Executable.Lens\n    Distribution.Types.ExecutableScope\n    Distribution.Types.ExposedModule\n    Distribution.Types.Flag\n    Distribution.Types.ForeignLib\n    Distribution.Types.ForeignLib.Lens\n    Distribution.Types.ForeignLibOption\n    Distribution.Types.ForeignLibType\n    Distribution.Types.GenericPackageDescription\n    Distribution.Types.GenericPackageDescription.Lens\n    Distribution.Types.HookedBuildInfo\n    Distribution.Types.IncludeRenaming\n    Distribution.Types.InstalledPackageInfo\n    Distribution.Types.InstalledPackageInfo.Lens\n    Distribution.Types.InstalledPackageInfo.FieldGrammar\n    Distribution.Types.LegacyExeDependency\n    Distribution.Types.Lens\n    Distribution.Types.Library\n    Distribution.Types.Library.Lens\n    Distribution.Types.LibraryName\n    Distribution.Types.LibraryVisibility\n    Distribution.Types.Mixin\n    Distribution.Types.Module\n    Distribution.Types.ModuleReexport\n    Distribution.Types.ModuleRenaming\n    Distribution.Types.MungedPackageId\n    Distribution.Types.MungedPackageName\n    Distribution.Types.PackageDescription\n    Distribution.Types.PackageDescription.Lens\n    Distribution.Types.PackageId\n    Distribution.Types.PackageId.Lens\n    Distribution.Types.PackageName\n    Distribution.Types.PackageVersionConstraint\n    Distribution.Types.PkgconfigDependency\n    Distribution.Types.PkgconfigName\n    Distribution.Types.PkgconfigVersion\n    Distribution.Types.PkgconfigVersionRange\n    Distribution.Types.SetupBuildInfo\n    Distribution.Types.SetupBuildInfo.Lens\n    Distribution.Types.SourceRepo\n    Distribution.Types.SourceRepo.Lens\n    Distribution.Types.TestSuite\n    Distribution.Types.TestSuite.Lens\n    Distribution.Types.TestSuiteInterface\n    Distribution.Types.TestType\n    Distribution.Types.UnitId\n    Distribution.Types.UnqualComponentName\n    Distribution.Types.Version\n    Distribution.Types.VersionInterval\n    Distribution.Types.VersionInterval.Legacy\n    Distribution.Types.VersionRange\n    Distribution.Types.VersionRange.Internal\n    Distribution.Utils.Base62\n    Distribution.Utils.Generic\n    Distribution.Utils.MD5\n    Distribution.Utils.Path\n    Distribution.Utils.ShortText\n    Distribution.Utils.String\n    Distribution.Utils.Structured\n    Distribution.Version\n    Language.Haskell.Extension\n\n  other-extensions:\n    BangPatterns\n    CPP\n    DefaultSignatures\n    DeriveDataTypeable\n    DeriveFoldable\n    DeriveFunctor\n    DeriveGeneric\n    DeriveTraversable\n    ExistentialQuantification\n    FlexibleContexts\n    FlexibleInstances\n    GeneralizedNewtypeDeriving\n    ImplicitParams\n    KindSignatures\n    NondecreasingIndentation\n    OverloadedStrings\n    PatternSynonyms\n    RankNTypes\n    RecordWildCards\n    ScopedTypeVariables\n    StandaloneDeriving\n    Trustworthy\n    TypeFamilies\n    TypeOperators\n    TypeSynonymInstances\n    UndecidableInstances\n";
  }