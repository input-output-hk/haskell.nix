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
      identifier = { name = "basement"; version = "0.0.15"; };
      license = "BSD-3-Clause";
      copyright = "2015-2017 Vincent Hanquez <vincent@snarc.org>\n, 2017-2018 Foundation Maintainers";
      maintainer = "vincent@snarc.org";
      author = "";
      homepage = "https://github.com/haskell-foundation/foundation#readme";
      url = "";
      synopsis = "Foundation scrap box of array & string";
      description = "Foundation most basic primitives without any dependencies";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = (pkgs.lib).optionals (!(compiler.isGhc && (compiler.version).lt "8.10")) ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."ghc-prim" or (errorHandler.buildDepError "ghc-prim"))
          ] ++ (pkgs.lib).optional (system.isWindows) (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")));
        buildable = if compiler.isGhc && (compiler.version).lt "8.10"
          then false
          else true;
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/basement-0.0.15.tar.gz";
      sha256 = "56c2afb6754e8d16a627331a8a5d473b3d26fd04ba5c9e0fb3475f595b915db4";
      });
    }) // {
    package-description-override = "name:                basement\nversion:             0.0.15\nsynopsis:            Foundation scrap box of array & string\ndescription:         Foundation most basic primitives without any dependencies\nlicense:             BSD3\nlicense-file:        LICENSE\ncopyright:           2015-2017 Vincent Hanquez <vincent@snarc.org>\n                   , 2017-2018 Foundation Maintainers\nmaintainer:          vincent@snarc.org\ncategory:            Web\nbuild-type:          Simple\nhomepage:            https://github.com/haskell-foundation/foundation#readme\nbug-reports:         https://github.com/haskell-foundation/foundation/issues\ncabal-version:       1.18\nextra-source-files:  cbits/*.h\n\nsource-repository head\n  type: git\n  location: https://github.com/haskell-foundation/foundation\n  subdir: basement\n\nlibrary\n  hs-source-dirs:    .\n  exposed-modules:\n                     Basement.Imports\n\n                     Basement.Base16\n                     Basement.Bindings.Memory\n                     Basement.Endianness\n                     Basement.Environment\n                     Basement.PrimType\n\n                     Basement.Exception\n                     Basement.Cast\n                     Basement.From\n\n                     Basement.Types.Char7\n                     Basement.Types.CharUTF8\n                     Basement.Types.OffsetSize\n                     Basement.Types.Ptr\n                     Basement.Types.AsciiString\n                     Basement.Types.Word128\n                     Basement.Types.Word256\n                     Basement.Monad\n                     Basement.MutableBuilder\n                     Basement.FinalPtr\n\n                     Basement.Nat\n\n                     -- Extended Types\n                     Basement.BoxedArray\n                     Basement.Block\n                     Basement.Block.Mutable\n                     Basement.Block.Builder\n                     Basement.UArray\n                     Basement.UArray.Mutable\n                     Basement.String\n                     Basement.String.Builder\n                     Basement.NonEmpty\n\n                     -- Extended Types with explicit type level size\n                     Basement.Sized.Block\n                     Basement.Sized.UVect\n                     Basement.Sized.Vect\n                     Basement.Sized.List\n                     Basement.BlockN\n\n                     -- Utils\n                     Basement.NormalForm\n                     Basement.These\n\n                     -- Terminal\n                     Basement.Terminal\n                     Basement.Terminal.ANSI\n\n                     -- numeric stuff\n                     Basement.IntegralConv\n                     Basement.Floating\n                     Basement.Numerical.Number\n                     Basement.Numerical.Additive\n                     Basement.Numerical.Subtractive\n                     Basement.Numerical.Multiplicative\n                     Basement.Bounded\n\n                     -- exported algorithms\n                     Basement.Alg.XorShift\n\n                     -- compat / base redefinition\n                     Basement.Compat.AMP\n                     Basement.Compat.Base\n                     Basement.Compat.Bifunctor\n                     Basement.Compat.CallStack\n                     Basement.Compat.C.Types\n                     Basement.Compat.ExtList\n                     Basement.Compat.IsList\n                     Basement.Compat.Identity\n                     Basement.Compat.Primitive\n                     Basement.Compat.PrimTypes\n                     Basement.Compat.MonadTrans\n                     Basement.Compat.Semigroup\n                     Basement.Compat.Natural\n                     Basement.Compat.NumLiteral\n                     Basement.Compat.Typeable\n\n                     Basement.Bits\n\n  other-modules:\n                     Basement.Error\n                     Basement.Show\n                     Basement.Runtime\n\n                     Basement.Alg.Class\n                     Basement.Alg.Mutable\n                     Basement.Alg.PrimArray\n\n                     Basement.Alg.UTF8\n                     Basement.Alg.String\n\n                     Basement.Numerical.Conversion\n\n                     Basement.Block.Base\n\n                     Basement.UTF8.Base\n                     Basement.UTF8.Helper\n                     Basement.UTF8.Table\n                     Basement.UTF8.Types\n\n                     Basement.UArray.Base\n\n                     Basement.String.CaseMapping\n                     Basement.String.Encoding.Encoding\n                     Basement.String.Encoding.UTF16\n                     Basement.String.Encoding.UTF32\n                     Basement.String.Encoding.ASCII7\n                     Basement.String.Encoding.ISO_8859_1\n\n                     Basement.Terminal.Size\n\n  -- support and dependencies\n  if impl(ghc < 8.10)\n    buildable: False\n  else\n    build-depends:     base\n                     , ghc-prim\n    if os(windows)\n      build-depends:   Win32\n\n  default-language:    Haskell2010\n  default-extensions: NoImplicitPrelude\n                      RebindableSyntax\n                      TypeFamilies\n                      BangPatterns\n                      DeriveDataTypeable\n  if (arch(i386) || arch(x86_64))\n    cpp-options: -DARCH_IS_LITTLE_ENDIAN\n  else\n    cpp-options: -DARCH_IS_UNKNOWN_ENDIAN\n  include-dirs:      cbits\n  c-sources:         cbits/foundation_mem.c\n";
    }