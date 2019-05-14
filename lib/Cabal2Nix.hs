{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Cabal2Nix (cabal2nix, gpd2nix, Src(..), CabalFile(..), CabalFileGenerator(..), cabalFilePath, cabalFilePkgName) where

import Distribution.PackageDescription.Parsec (readGenericPackageDescription, parseGenericPackageDescription, runParseResult)
import Distribution.Verbosity (normal)
import Distribution.Text (disp)
import Distribution.Pretty (pretty)
import Data.Char (toUpper)
import System.FilePath
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes)

import Distribution.Types.CondTree
import Distribution.Types.Library
import Distribution.Types.ForeignLib
import Distribution.PackageDescription hiding (Git)
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.LegacyExeDependency
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Types.VersionRange
import Distribution.Compiler
import Distribution.Types.PackageName (PackageName, mkPackageName)
import Distribution.Simple.BuildToolDepends (desugarBuildTool)

import Data.String (fromString, IsString)

-- import Distribution.Types.GenericPackageDescription
-- import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
--import Distribution.Types.Condition
import Distribution.Types.UnqualComponentName
import Data.List.NonEmpty (NonEmpty(..))
import Nix.Expr
import Data.Fix(Fix(..))
import Data.Text (Text)

import Cabal2Nix.Util (quoted)

data Src
  = Path FilePath
  | Git String String (Maybe String) (Maybe String)
  deriving Show

pkgs, hsPkgs, pkgconfPkgs, flags :: Text
pkgs   = "pkgs"
hsPkgs = "hsPkgs"
pkgconfPkgs = "pkgconfPkgs"
flags  = "flags"

($//?) :: NExpr -> Maybe NExpr -> NExpr
lhs $//? (Just e) = lhs $// e
lhs $//? Nothing  = lhs

data CabalFileGenerator
  = Hpack
  deriving Show

data CabalFile
  = OnDisk FilePath
  | InMemory (Maybe CabalFileGenerator) FilePath ByteString
  deriving Show


cabalFilePath :: CabalFile -> String
cabalFilePath (OnDisk fp) = fp
cabalFilePath (InMemory _ fp _) = fp

cabalFilePkgName :: CabalFile -> String
cabalFilePkgName = dropExtension . takeFileName . cabalFilePath

genExtra :: CabalFileGenerator -> NExpr
genExtra Hpack = mkNonRecSet [ "cabal-generator" $= mkStr "hpack" ]

cabal2nix :: Maybe Src -> CabalFile -> IO NExpr
cabal2nix src = \case
  (OnDisk path) -> gpd2nix src Nothing
    <$> readGenericPackageDescription normal path
  (InMemory gen _ body) -> gpd2nix src (genExtra <$> gen)
    <$> case runParseResult (parseGenericPackageDescription body) of
        (_, Left (_, err)) -> error ("Failed to parse in-memory cabal file: " ++ show err)
        (_, Right desc) -> pure desc

gpd2nix :: Maybe Src -> Maybe NExpr -> GenericPackageDescription -> NExpr
gpd2nix src extra gpd = mkFunction args $ toNix gpd $//? (toNix <$> src) $//? extra
  where args :: Params NExpr
        args = mkParamset [ ("system", Nothing)
                          , ("compiler", Nothing)
                          , ("flags", Nothing)
                          , (pkgs, Nothing)
                          , (hsPkgs, Nothing)
                          , (pkgconfPkgs, Nothing)]
                          True

class HasBuildInfo a where
  getBuildInfo :: a -> BuildInfo

instance HasBuildInfo Library where
  getBuildInfo = libBuildInfo

instance HasBuildInfo ForeignLib where
  getBuildInfo = foreignLibBuildInfo

instance HasBuildInfo Executable where
  getBuildInfo = buildInfo

instance HasBuildInfo TestSuite where
  getBuildInfo = testBuildInfo

instance HasBuildInfo Benchmark where
  getBuildInfo = benchmarkBuildInfo

--- Clean the Tree from empty nodes
-- CondBranch is empty if the true and false branch are empty.
shakeTree :: (Foldable t, Foldable f) => CondTree v (t c) (f a) -> Maybe (CondTree v (t c) (f a))
shakeTree (CondNode d c bs) = case (null d, null bs') of
                                (True, True) -> Nothing
                                _            -> Just (CondNode d c bs')
  where bs' = catMaybes (shakeBranch <$> bs)

shakeBranch :: (Foldable t, Foldable f) => CondBranch v (t c) (f a) -> Maybe (CondBranch v (t c) (f a))
shakeBranch (CondBranch c t f) = case (shakeTree t, f >>= shakeTree) of
  (Nothing, Nothing) -> Nothing
  (Nothing, Just f') -> shakeBranch (CondBranch (CNot c) f' Nothing)
  (Just t', f') -> Just (CondBranch c t' f')

--- String helper
transformFst :: (Char -> Char) -> String -> String
transformFst _ [] = []
transformFst f (x:xs) = f x : xs
capitalize :: String -> String
capitalize = transformFst toUpper

--- Turn something into a NExpr

class ToNixExpr a where
  toNix :: a -> NExpr

class ToNixBinding a where
  toNixBinding :: a -> Binding NExpr

instance ToNixExpr Src where
  toNix (Path p) = mkRecSet [ "src" $= applyMkDefault (mkRelPath p) ]
  toNix (Git url rev mbSha256 mbPath)
    = mkNonRecSet $
      [ "src" $= applyMkDefault (mkSym pkgs @. "fetchgit" @@ mkNonRecSet
        [ "url"    $= mkStr (fromString url)
        , "rev"    $= mkStr (fromString rev)
        , "sha256" $= case mbSha256 of
                        Just sha256 -> mkStr (fromString sha256)
                        Nothing     -> mkNull
        ])
      ] <>
      [ "postUnpack"
        $= mkStr (fromString $ "sourceRoot+=/" <> root <> "; echo source root reset to $sourceRoot")
      | Just root <- [mbPath] ]

applyMkDefault :: NExpr -> NExpr
applyMkDefault expr = mkSym pkgs @. "lib" @. "mkDefault" @@ expr

instance ToNixExpr PackageIdentifier where
  toNix ident = mkNonRecSet [ "name"    $= mkStr (fromString (show (disp (pkgName ident))))
                            , "version" $= mkStr (fromString (show (disp (pkgVersion ident))))]

instance ToNixExpr PackageDescription where
  toNix pd = mkNonRecSet $ [ "specVersion" $= mkStr (fromString (show (disp (specVersion pd))))
                           , "identifier"  $= toNix (package pd)
                           , "license"     $= mkStr (fromString (show (pretty (license pd))))

                           , "copyright"   $= mkStr (fromString (copyright pd))
                           , "maintainer"  $= mkStr (fromString (maintainer pd))
                           , "author"      $= mkStr (fromString (author pd))

                           , "homepage"    $= mkStr (fromString (homepage pd))
                           , "url"         $= mkStr (fromString (pkgUrl pd))

                           , "synopsis"    $= mkStr (fromString (synopsis pd))
                           , "description" $= mkStr (fromString (description pd))

                           , "buildType"   $= mkStr (fromString (show (pretty (buildType pd))))
                           ] ++
                           [ "setup-depends" $= toNix (BuildToolDependency . depPkgName <$> deps) | Just deps <- [setupDepends <$> setupBuildInfo pd ]]

newtype SysDependency = SysDependency { unSysDependency :: String } deriving (Show, Eq, Ord)
newtype BuildToolDependency = BuildToolDependency { unBuildToolDependency :: PackageName } deriving (Show, Eq, Ord)

mkSysDep :: String -> SysDependency
mkSysDep = SysDependency

instance ToNixExpr GenericPackageDescription where
  toNix gpd = mkNonRecSet [ "flags"         $= (mkNonRecSet . fmap toNixBinding $ genPackageFlags gpd)
                          , "package"       $= toNix (packageDescription gpd)
                          , "components"    $= components ]
    where _packageName :: IsString a => a
          _packageName = fromString . show . disp . pkgName . package . packageDescription $ gpd
          component unQualName comp
            = quoted name $=
                      mkNonRecSet ([ "depends"    $= toNix deps | Just deps <- [shakeTree . fmap (         targetBuildDepends . getBuildInfo) $ comp ] ] ++
                                   [ "libs"       $= toNix deps | Just deps <- [shakeTree . fmap (  fmap mkSysDep . extraLibs . getBuildInfo) $ comp ] ] ++
                                   [ "frameworks" $= toNix deps | Just deps <- [shakeTree . fmap ( fmap mkSysDep . frameworks . getBuildInfo) $ comp ] ] ++
                                   [ "pkgconfig"  $= toNix deps | Just deps <- [shakeTree . fmap (           pkgconfigDepends . getBuildInfo) $ comp ] ] ++
                                   [ "build-tools"$= toNix deps | Just deps <- [shakeTree . fmap (                   toolDeps . getBuildInfo) $ comp ] ])
              where name = fromString $ unUnqualComponentName unQualName
                    toolDeps = getToolDependencies (packageDescription gpd)
                    toBuildToolDep (ExeDependency pkg _ _) = BuildToolDependency pkg
                    getToolDependencies pkg bi =
                           map toBuildToolDep (buildToolDepends bi)
                        <> map (\led -> maybe (guess led) toBuildToolDep $ desugarBuildTool pkg led) (buildTools bi)
                    guess (LegacyExeDependency n _) = BuildToolDependency (mkPackageName n)
          components = mkNonRecSet $
            [ component "library" lib | Just lib <- [condLibrary gpd] ] ++
            (bindTo "sublibs"     . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condSubLibraries gpd ]) ++
            (bindTo "foreignlibs" . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condForeignLibs  gpd ]) ++
            (bindTo "exes"        . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condExecutables  gpd ]) ++
            (bindTo "tests"       . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condTestSuites   gpd ]) ++
            (bindTo "benchmarks"  . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condBenchmarks   gpd ])

instance ToNixExpr Dependency where
  toNix = (@.) (mkSym hsPkgs) . fromString . show . pretty . depPkgName

instance ToNixExpr SysDependency where
  toNix = (@.) (mkSym pkgs) . quoted . fromString . unSysDependency

instance ToNixExpr PkgconfigDependency where
  toNix (PkgconfigDependency name _versionRange)= (@.) (mkSym pkgconfPkgs) . quoted . fromString . unPkgconfigName $ name

instance ToNixExpr ExeDependency where
  toNix (ExeDependency pkgName' _unqualCompName _versionRange) = mkSym . fromString . show . pretty $ pkgName'

instance ToNixExpr BuildToolDependency where
  toNix (BuildToolDependency pkgName') =
      -- TODO once https://github.com/haskell-nix/hnix/issues/52
      -- is reolved use something like:
      -- [nix| hsPkgs.buildPackages.$((pkgName)) or pkgs.buildPackages.$((pkgName)) ]
      Fix $ NSelect (mkSym hsPkgs) buildPackagesDotName
        (Just . Fix $ NSelect (mkSym pkgs) buildPackagesDotName Nothing)
    where
      buildPackagesDotName = StaticKey "buildPackages" :| [StaticKey (fromString . show . pretty $ pkgName')]

instance ToNixExpr LegacyExeDependency where
  toNix (LegacyExeDependency name _versionRange) = mkSym hsPkgs @. fromString name

instance {-# OVERLAPPABLE #-} ToNixExpr String where
  toNix = mkStr . fromString

instance {-# OVERLAPS #-} ToNixExpr a => ToNixExpr [a] where
  toNix = mkList . fmap toNix

instance ToNixExpr ConfVar where
  toNix (OS os) = mkSym "system" @. (fromString . ("is" ++) . capitalize . show . pretty $ os)
  toNix (Arch arch) = mkSym "system" @. (fromString . ("is" ++) . capitalize . show . pretty $ arch)
  toNix (Flag flag) = mkSym flags @. (fromString . show . pretty $ flag)
  toNix (Impl flavour range) = toNix flavour $&& toNix (projectVersionRange range)

instance ToNixExpr CompilerFlavor where
  toNix flavour = mkSym "compiler" @. (fromString . ("is" ++) . capitalize . show . pretty $ flavour)

instance ToNixExpr (VersionRangeF VersionRange) where
  toNix AnyVersionF              = mkBool True
  toNix (ThisVersionF       ver) = mkSym "compiler" @. "version" @. "eq" @@ mkStr (fromString (show (disp ver)))
  toNix (LaterVersionF      ver) = mkSym "compiler" @. "version" @. "gt" @@ mkStr (fromString (show (disp ver)))
  toNix (OrLaterVersionF    ver) = mkSym "compiler" @. "version" @. "ge" @@ mkStr (fromString (show (disp ver)))
  toNix (EarlierVersionF    ver) = mkSym "compiler" @. "version" @. "lt" @@ mkStr (fromString (show (disp ver)))
  toNix (OrEarlierVersionF  ver) = mkSym "compiler" @. "version" @. "le" @@ mkStr (fromString (show (disp ver)))
  toNix (WildcardVersionF  _ver) = mkBool False
--  toNix (MajorBoundVersionF ver) = mkSym "compiler" @. "version" @. "eq" @@ mkStr (fromString (show (disp ver)))
  toNix (IntersectVersionRangesF v1 v2) = toNix (projectVersionRange v1) $&& toNix (projectVersionRange v2)
  toNix x = error $ "ToNixExpr VersionRange for `" ++ show x ++ "` not implemented!"

instance ToNixExpr a => ToNixExpr (Condition a) where
  toNix (Var a) = toNix a
  toNix (Lit b) = mkBool b
  toNix (CNot c) = mkNot (toNix c)
  toNix (COr l r) = toNix l $|| toNix r
  toNix (CAnd l r) = toNix l $&& toNix r

instance (Foldable t, ToNixExpr (t a), ToNixExpr v, ToNixExpr c) => ToNixExpr (CondBranch v c (t a)) where
  toNix (CondBranch c t Nothing) = case toNix t of
    (Fix (NList [e])) -> mkSym pkgs @. "lib" @. "optional" @@ toNix c @@ e
    e -> mkSym pkgs @. "lib" @. "optionals" @@ toNix c @@ e
  toNix (CondBranch _c t (Just f)) | toNix t == toNix f = toNix t
  toNix (CondBranch c  t (Just f)) = mkIf (toNix c) (toNix t) (toNix f)

instance (Foldable t, ToNixExpr (t a), ToNixExpr v, ToNixExpr c) => ToNixExpr (CondTree v c (t a)) where
  toNix (CondNode d _c []) = toNix d
  toNix (CondNode d _c bs) | null d = foldl1 ($++) (fmap toNix bs)
                           | otherwise = foldl ($++) (toNix d) (fmap toNix bs)

instance ToNixBinding Flag where
  toNixBinding (MkFlag name _desc def _manual) = (fromString . show . pretty $ name) $= mkBool def
