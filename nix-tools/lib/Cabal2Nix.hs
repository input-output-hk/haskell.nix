{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cabal2Nix (cabal2nix, gpd2nix, Src(..), CabalFile(..), CabalFileGenerator(..), cabalFilePath, cabalFilePkgName, CabalDetailLevel(..)) where

import Distribution.PackageDescription.Parsec (readGenericPackageDescription, parseGenericPackageDescription, runParseResult)
import Distribution.Verbosity (normal)
import Distribution.Pretty (pretty)
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Utils.Path (getSymbolicPath)
import Data.Char (toUpper)
import System.FilePath
import Data.ByteString (ByteString)
import Data.Maybe (catMaybes, maybeToList)
import Data.Foldable (toList)
import Distribution.Package
         ( packageName, packageVersion )
import Distribution.Pretty (prettyShow)
import qualified System.FilePath.Posix as FilePath.Posix
         ( combine, joinPath, splitDirectories )
import Network.URI
         ( URI(uriAuthority, uriPath), URIAuth(..), parseURI )

import Distribution.Types.CondTree
import Distribution.Types.Library
import Distribution.Types.ForeignLib
import Distribution.PackageDescription hiding (Git)
import Distribution.Types.Dependency
import Distribution.Types.ExeDependency
import Distribution.Types.LegacyExeDependency
import Distribution.Types.PkgconfigDependency
import Distribution.Types.PkgconfigName
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.CabalSpecVersion
import Distribution.Compiler
import Distribution.Types.PackageName (PackageName, mkPackageName, unPackageName)
import Distribution.Simple.BuildToolDepends (desugarBuildTool)
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName

import Data.String (fromString, IsString)

-- import Distribution.Types.GenericPackageDescription
-- import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
--import Distribution.Types.Condition
import Distribution.Types.UnqualComponentName
import Nix.Expr
import Data.Fix(Fix(..))
import Data.Text (Text)

import Cabal2Nix.Util (quoted, selectOr, mkThrow)

data Src
  = Path FilePath
  | PrivateHackage String
  | Git String String (Maybe String) (Maybe String)
  deriving Show

pkgs, hsPkgs, errorHandler, pkgconfPkgs, flags :: Text
pkgs   = "pkgs"
hsPkgs = "hsPkgs"
errorHandler = "errorHandler"
pkgconfPkgs = "pkgconfPkgs"
flags  = "flags"

buildDepError, sysDepError, pkgConfDepError, exeDepError, legacyExeDepError, buildToolDepError, setupDepError :: Text
buildDepError = "buildDepError"
sysDepError = "sysDepError"
pkgConfDepError = "pkgConfDepError"
exeDepError = "exeDepError"
legacyExeDepError = "legacyExeDepError"
buildToolDepError = "buildToolDepError"
setupDepError = "setupDepError"

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

data CabalDetailLevel = MinimalDetails | FullDetails deriving (Show, Eq)

cabal2nix :: Bool -> CabalDetailLevel -> Maybe Src -> CabalFile -> IO NExpr
cabal2nix isLocal fileDetails src = \case
  (OnDisk path) -> gpd2nix isLocal fileDetails src Nothing
    <$> readGenericPackageDescription normal path
  (InMemory gen _ body) -> gpd2nix isLocal fileDetails src (genExtra <$> gen)
    <$> case runParseResult (parseGenericPackageDescription body) of
        (_, Left (_, err)) -> error ("Failed to parse in-memory cabal file: " ++ show err)
        (_, Right desc) -> pure desc

gpd2nix :: Bool -> CabalDetailLevel -> Maybe Src -> Maybe NExpr -> GenericPackageDescription -> NExpr
gpd2nix isLocal fileDetails src extra gpd =
  mkFunction args $ toNixGenericPackageDescription isLocal fileDetails gpd
    $//? (srcToNix (package $ packageDescription gpd) <$> src)
    $//? extra
  where args :: Params NExpr
        args = mkParamset [ ("system", Nothing)
                          , ("compiler", Nothing)
                          , ("flags", Nothing)
                          , (pkgs, Nothing)
                          , (hsPkgs, Nothing)
                          , (pkgconfPkgs, Nothing)
                          , (errorHandler, Nothing)
                          , ("config", Nothing)]
                          True

class IsComponent a where
  getBuildInfo :: a -> BuildInfo
  getMainPath :: a -> Maybe FilePath
  getMainPath _ = Nothing
  modules :: a -> [ModuleName]
  modules = otherModules . getBuildInfo

instance IsComponent Library where
  getBuildInfo = libBuildInfo
  modules a = otherModules (getBuildInfo a)
      <> exposedModules a
      <> signatures a

instance IsComponent ForeignLib where
  getBuildInfo = foreignLibBuildInfo

instance IsComponent Executable where
  getBuildInfo = buildInfo
  getMainPath Executable {modulePath = p} = Just p

instance IsComponent TestSuite where
  getBuildInfo = testBuildInfo
  getMainPath TestSuite {testInterface = (TestSuiteExeV10 _ p)} = Just p
  getMainPath _ = Nothing

instance IsComponent Benchmark where
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

applyMkDefault :: NExpr -> NExpr
applyMkDefault expr = (mkSym pkgs @. "lib") @. "mkDefault" @@ expr

instance ToNixExpr PackageIdentifier where
  toNix ident = mkNonRecSet [ "name"    $= mkStr (fromString (show (pretty (pkgName ident))))
                            , "version" $= mkStr (fromString (show (pretty (pkgVersion ident))))]

toNixPackageDescription :: Bool -> CabalDetailLevel -> PackageDescription -> NExpr
toNixPackageDescription isLocal detailLevel pd = mkNonRecSet $
    [ "specVersion" $= mkStr (fromString (showCabalSpecVersion (specVersion pd)))
    , "identifier"  $= toNix (package pd)
    , "license"     $= mkStr (fromString (show (pretty (license pd))))

    , "copyright"   $= mkStr (fromString (fromShortText (copyright pd)))
    , "maintainer"  $= mkStr (fromString (fromShortText (maintainer pd)))
    , "author"      $= mkStr (fromString (fromShortText (author pd)))

    , "homepage"    $= mkStr (fromString (fromShortText (homepage pd)))
    , "url"         $= mkStr (fromString (fromShortText (pkgUrl pd)))

    , "synopsis"    $= mkStr (fromString (fromShortText (synopsis pd)))
    , "description" $= mkStr (fromString (fromShortText (description pd)))

    , "buildType"   $= mkStr (fromString (show (pretty (buildType pd))))
    ] ++
    [ "isLocal"     $= mkBool True | isLocal
    ] ++
    [ "setup-depends" $= toNix deps | Just deps <- [(>>= toSetupDepends) . setupDepends <$> setupBuildInfo pd ]] ++
    if detailLevel == MinimalDetails
      then []
      else
        [ "detailLevel"   $= mkStr (fromString (show detailLevel))
        , "licenseFiles"  $= toNix (map getSymbolicPath (licenseFiles pd))
        , "dataDir"       $= mkStr (fromString (dataDir pd))
        , "dataFiles"     $= toNix (dataFiles pd)
        , "extraSrcFiles" $= toNix (extraSrcFiles pd)
        , "extraTmpFiles" $= toNix (extraTmpFiles pd)
        , "extraDocFiles" $= toNix (extraDocFiles pd)
        ]
  where
    toSetupDepends (Dependency pkg _ libs) = SetupDependency pkg <$> toList libs

srcToNix :: PackageIdentifier -> Src -> NExpr
srcToNix _ (Path p) = mkRecSet [ "src" $= applyMkDefault (mkRelPath p) ]
srcToNix pi' (PrivateHackage url)
  = mkNonRecSet $
    [ "src" $= applyMkDefault (mkSym pkgs @. "fetchurl" @@ mkNonRecSet
      [ "url" $= mkStr (fromString . show $ mkPrivateHackageUrl url pi')
      , "sha256" $= (mkSym "config" @. "sha256")
      ])
    ]
srcToNix _ (Git url rev mbSha256 mbPath)
  = mkNonRecSet $
    [ "src" $= applyMkDefault (mkSym pkgs @. "fetchgit" @@ mkNonRecSet
      [ "url"    $= mkStr (fromString url)
      , "rev"    $= mkStr (fromString rev)
      , "sha256" $= case mbSha256 of
                      Just sha256 -> mkStr (fromString sha256)
                      Nothing     -> mkNull
      ]) $// mkNonRecSet
      [ "url"    $= mkStr (fromString url)
      , "rev"    $= mkStr (fromString rev)
      , "sha256" $= case mbSha256 of
                      Just sha256 -> mkStr (fromString sha256)
                      Nothing     -> mkNull
      ]
    ] <>
    [ "postUnpack"
      $= mkStr (fromString $ "sourceRoot+=/" <> root <> "; echo source root reset to $sourceRoot")
    | Just root <- [mbPath] ]

-- This logic is hard coded in `cabal-install` see:
-- * Distribution.Client.HttpUtils.isOldHackageURI
isOldHackageURI :: URI -> Bool
isOldHackageURI uri
    = case uriAuthority uri of
        Just (URIAuth {uriRegName = "hackage.haskell.org"}) ->
            FilePath.Posix.splitDirectories (uriPath uri)
            == ["/","packages","archive"]
        _ -> False

-- This logic is hard coded in `cabal-install` see:
-- * Distribution.Client.FetchUtils.packageURI
packageURI :: URI -> PackageId -> URI
packageURI remoteRepoURI pkgid | isOldHackageURI remoteRepoURI =
  remoteRepoURI {
    uriPath = FilePath.Posix.joinPath
      [uriPath remoteRepoURI
      ,prettyShow (packageName    pkgid)
      ,prettyShow (packageVersion pkgid)
      ,prettyShow pkgid <.> "tar.gz"]
  }
packageURI remoteRepoURI pkgid =
  remoteRepoURI {
    uriPath = FilePath.Posix.joinPath
      [uriPath remoteRepoURI
      ,"package"
      ,prettyShow pkgid <.> "tar.gz"]
  }

mkPrivateHackageUrl :: String -> PackageIdentifier -> URI
mkPrivateHackageUrl hackageUrl = maybe
  (error $ "Unable to parse hackage URI " <> hackageUrl)
  packageURI
  (parseURI hackageUrl)

newtype SysDependency = SysDependency { unSysDependency :: String } deriving (Show, Eq, Ord)
data SetupDependency = SetupDependency PackageName LibraryName deriving (Show, Eq, Ord)
data HaskellLibDependency = HaskellLibDependency PackageName LibraryName deriving (Show, Eq, Ord)
data BuildToolDependency = BuildToolDependency PackageName UnqualComponentName deriving (Show, Eq, Ord)

mkSysDep :: String -> SysDependency
mkSysDep = SysDependency

toNixGenericPackageDescription :: Bool -> CabalDetailLevel -> GenericPackageDescription -> NExpr
toNixGenericPackageDescription isLocal detailLevel gpd = mkNonRecSet
                          [ "flags"         $= (mkNonRecSet . fmap toNixBinding $ genPackageFlags gpd)
                          , "package"       $= toNixPackageDescription isLocal detailLevel (packageDescription gpd)
                          , "components"    $= components ]
    where _packageName :: IsString a => a
          _packageName = fromString . show . pretty . pkgName . package . packageDescription $ gpd
          component :: IsComponent comp => UnqualComponentName -> CondTree ConfVar [Dependency] comp -> Binding NExpr
          component unQualName comp
            = quoted name $=
                mkNonRecSet (
                  [ "depends"      $= toNix deps | Just deps <- [shakeTree . fmap ( (>>= depends) . targetBuildDepends . getBuildInfo) $ comp ] ] ++
                  [ "libs"         $= toNix deps | Just deps <- [shakeTree . fmap (  fmap mkSysDep . extraLibs . getBuildInfo) $ comp ] ] ++
                  [ "frameworks"   $= toNix deps | Just deps <- [shakeTree . fmap ( fmap mkSysDep . frameworks . getBuildInfo) $ comp ] ] ++
                  [ "pkgconfig"    $= toNix deps | Just deps <- [shakeTree . fmap (           pkgconfigDepends . getBuildInfo) $ comp ] ] ++
                  [ "build-tools"  $= toNix deps | Just deps <- [shakeTree . fmap (                   toolDeps . getBuildInfo) $ comp ] ] ++
                  [ "buildable"    $= boolTreeToNix (and <$> b) | Just b <- [shakeTree . fmap ((:[]) . buildable . getBuildInfo) $ comp ] ] ++
                  if detailLevel == MinimalDetails
                    then []
                    else
                      [ "modules"      $= toNix mods | Just mods <- [shakeTree . fmap (fmap ModuleName.toFilePath . modules) $ comp ] ] ++
                      [ "asmSources"   $= toNix src  | Just src  <- [shakeTree . fmap (asmSources   . getBuildInfo) $ comp ] ] ++
                      [ "cmmSources"   $= toNix src  | Just src  <- [shakeTree . fmap (cmmSources   . getBuildInfo) $ comp ] ] ++
                      [ "cSources"     $= toNix src  | Just src  <- [shakeTree . fmap (cSources     . getBuildInfo) $ comp ] ] ++
                      [ "cxxSources"   $= toNix src  | Just src  <- [shakeTree . fmap (cxxSources   . getBuildInfo) $ comp ] ] ++
                      [ "jsSources"    $= toNix src  | Just src  <- [shakeTree . fmap (jsSources    . getBuildInfo) $ comp ] ] ++
                      [ "hsSourceDirs" $= toNix (fmap getSymbolicPath <$> dir) | Just dir  <- [shakeTree . fmap (hsSourceDirs . getBuildInfo) $ comp ] ] ++
                      [ "includeDirs"  $= toNix dir  | Just dir  <- [shakeTree . fmap (includeDirs  . getBuildInfo) $ comp] ] ++
                      [ "includes"     $= toNix dir  | Just dir  <- [shakeTree . fmap (includes     . getBuildInfo) $ comp] ] ++
                      [ "mainPath"     $= toNix p | Just p <- [shakeTree . fmap (maybeToList . getMainPath) $ comp] ])
              where name = fromString $ unUnqualComponentName unQualName
                    depends (Dependency pkg _ libs) = HaskellLibDependency pkg <$> toList libs
                    toolDeps = getToolDependencies (packageDescription gpd)
                    toBuildToolDep (ExeDependency pkg c _) = BuildToolDependency pkg c
                    getToolDependencies pkg bi =
                           map toBuildToolDep (buildToolDepends bi)
                        <> map (\led -> maybe (guess led) toBuildToolDep $ desugarBuildTool pkg led) (buildTools bi)
                    guess (LegacyExeDependency n _) = BuildToolDependency (mkPackageName n) (mkUnqualComponentName n)
          components = mkNonRecSet $
            [ component "library" lib | Just lib <- [condLibrary gpd] ] ++
            (bindTo "sublibs"     . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condSubLibraries gpd ]) ++
            (bindTo "foreignlibs" . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condForeignLibs  gpd ]) ++
            (bindTo "exes"        . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condExecutables  gpd ]) ++
            (bindTo "tests"       . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condTestSuites   gpd ]) ++
            (bindTo "benchmarks"  . mkNonRecSet <$> filter (not . null) [ uncurry component <$> condBenchmarks   gpd ])

-- WARNING: these use functions bound at he top level in the GPD expression, they won't work outside it

instance ToNixExpr Dependency where
  toNix d = selectOr (mkSym hsPkgs) (mkSelector $ quoted pkg) (mkSym errorHandler @. buildDepError @@ mkStr pkg)
    where
      pkg = fromString . show . pretty . depPkgName $ d

instance ToNixExpr HaskellLibDependency where
  toNix (HaskellLibDependency p LMainLibName) = selectOr (mkSym hsPkgs) (
             mkSelector (quoted pkg))
      (mkSym errorHandler @. buildDepError @@ mkStr pkg)
    where
      pkg = fromString . show $ pretty p
  toNix (HaskellLibDependency p (LSubLibName l)) = selectOr (mkSym hsPkgs) (
             mkSelector (quoted pkg)
          <> mkSelector "components"
          <> mkSelector "sublibs"
          <> mkSelector lName)
      (mkSym errorHandler @. buildDepError @@ mkStr (pkg <> ":" <> lName))
    where
      pkg = fromString . show $ pretty p
      lName = fromString $ unUnqualComponentName l

instance ToNixExpr SysDependency where
  toNix d = selectOr (mkSym pkgs) (mkSelector $ quoted pkg) (mkSym errorHandler @. sysDepError @@ mkStr pkg)
    where
      pkg = fromString . unSysDependency $ d

instance ToNixExpr PkgconfigDependency where
  toNix (PkgconfigDependency name _versionRange) = selectOr (mkSym pkgconfPkgs) (mkSelector $ quoted pkg) (mkSym errorHandler @. pkgConfDepError @@ mkStr pkg)
    where
      pkg = fromString . unPkgconfigName $ name

instance ToNixExpr ExeDependency where
  toNix (ExeDependency pkgName' _unqualCompName _versionRange) = selectOr (mkSym "exes") (mkSelector $ pkg) (mkSym errorHandler @. exeDepError @@ mkStr pkg)
    where
      pkg = fromString . show . pretty $ pkgName'

instance ToNixExpr SetupDependency where
  toNix (SetupDependency pkgName' LMainLibName) =
      -- TODO once https://github.com/haskell-nix/hnix/issues/52
      -- is reolved use something like:
      -- [nix| hsPkgs.buildPackages.$((pkgName)) or pkgs.buildPackages.$((pkgName)) ]
      selectOr (mkSym hsPkgs) buildPackagesDotName
        (selectOr (mkSym pkgs) buildPackagesDotName (mkSym errorHandler @. setupDepError @@ mkStr pkg))
    where
      pkg = fromString . show . pretty $ pkgName'
      buildPackagesDotName = mkSelector "buildPackages" <> mkSelector pkg
  toNix (SetupDependency pkgName' (LSubLibName l)) = selectOr (mkSym hsPkgs) (
             mkSelector "buildPackages"
          <> mkSelector (quoted pkg)
          <> mkSelector "components"
          <> mkSelector "sublibs"
          <> mkSelector lName)
      (mkSym errorHandler @. setupDepError @@ mkStr (pkg <> ":" <> lName))
    where
      pkg = fromString . show $ pretty pkgName'
      lName = fromString $ unUnqualComponentName l

instance ToNixExpr BuildToolDependency where
  toNix (BuildToolDependency pkgName' componentName') =
      selectOr (mkSym hsPkgs) (
             mkSelector "buildPackages"
          <> mkSelector pkg
          <> mkSelector "components"
          <> mkSelector "exes"
          <> mkSelector componentName)
        (selectOr (mkSym pkgs) (mkSelector "buildPackages" <> mkSelector componentName)
          (mkSym errorHandler @. buildToolDepError @@ mkStr (pkg <> ":" <> componentName)))
    where
      pkg = fromString . show . pretty $ pkgName'
      componentName = fromString . show . pretty $ componentName'

instance ToNixExpr LegacyExeDependency where
  toNix (LegacyExeDependency name _versionRange) = selectOr (mkSym hsPkgs) (mkSelector $ quoted pkg) (mkSym errorHandler @. legacyExeDepError @@ mkStr pkg)
    where
      pkg = fromString name

instance {-# OVERLAPPABLE #-} ToNixExpr String where
  toNix = mkStr . fromString

instance {-# OVERLAPS #-} ToNixExpr a => ToNixExpr [a] where
  toNix = mkList . fmap toNix

instance ToNixExpr ConfVar where
  toNix (OS os) = mkSym "system" @. (fromString . ("is" ++) . capitalize . show . pretty $ os)
  toNix (Arch arch) = mkSym "system" @. (fromString . ("is" ++) . capitalize . show . pretty $ arch)
  toNix (PackageFlag flag) = mkSym flags @. (fromString . show . pretty $ flag)
  toNix (Impl flavour range) = toNix flavour $&& toNix (projectVersionRange range)

instance ToNixExpr CompilerFlavor where
  toNix flavour = mkSym "compiler" @. (fromString . ("is" ++) . capitalize . show . pretty $ flavour)

instance ToNixExpr (VersionRangeF VersionRange) where
  toNix (OrLaterVersionF    ver) | ver == version0 = mkBool True
  toNix (ThisVersionF       ver) = (mkSym "compiler" @. "version") @. "eq" @@ mkStr (fromString (show (pretty ver)))
  toNix (LaterVersionF      ver) = (mkSym "compiler" @. "version") @. "gt" @@ mkStr (fromString (show (pretty ver)))
  toNix (OrLaterVersionF    ver) = (mkSym "compiler" @. "version") @. "ge" @@ mkStr (fromString (show (pretty ver)))
  toNix (EarlierVersionF    ver) = (mkSym "compiler" @. "version") @. "lt" @@ mkStr (fromString (show (pretty ver)))
  toNix (OrEarlierVersionF  ver) = (mkSym "compiler" @. "version") @. "le" @@ mkStr (fromString (show (pretty ver)))
  toNix (MajorBoundVersionF ver) = toNix (IntersectVersionRangesF (orLaterVersion ver) (earlierVersion (majorUpperBound ver)))
  toNix (IntersectVersionRangesF v1 v2) = toNix (projectVersionRange v1) $&& toNix (projectVersionRange v2)
  toNix (UnionVersionRangesF v1 v2) = toNix (projectVersionRange v1) $|| toNix (projectVersionRange v2)

instance ToNixExpr a => ToNixExpr (Condition a) where
  toNix (Var a) = toNix a
  toNix (Lit b) = mkBool b
  toNix (CNot c) = mkNot (toNix c)
  toNix (COr l r) = toNix l $|| toNix r
  toNix (CAnd l r) = toNix l $&& toNix r

instance (Foldable t, ToNixExpr (t a), ToNixExpr v, ToNixExpr c) => ToNixExpr (CondBranch v c (t a)) where
  toNix (CondBranch c t Nothing) = case toNix t of
    (Fix (NList [e])) -> (mkSym pkgs @. "lib") @. "optional" @@ toNix c @@ e
    e -> (mkSym pkgs @. "lib") @. "optionals" @@ toNix c @@ e
  toNix (CondBranch _c t (Just f)) | toNix t == toNix f = toNix t
  toNix (CondBranch c  t (Just f)) = mkIf (toNix c) (toNix t) (toNix f)

instance (Foldable t, ToNixExpr (t a), ToNixExpr v, ToNixExpr c) => ToNixExpr (CondTree v c (t a)) where
  toNix (CondNode d _c []) = toNix d
  toNix (CondNode d _c bs) | null d = foldl1 ($++) (fmap toNix bs)
                           | otherwise = foldl ($++) (toNix d) (fmap toNix bs)

boolBranchToNix :: (ToNixExpr v, ToNixExpr c) => CondBranch v c Bool -> NExpr
boolBranchToNix (CondBranch _c t Nothing) | boolTreeToNix t == mkBool True = mkBool True
boolBranchToNix (CondBranch c  t Nothing) = mkIf (toNix c) (boolTreeToNix t) (mkBool True)
boolBranchToNix (CondBranch _c t (Just f)) | boolTreeToNix t == boolTreeToNix f = boolTreeToNix t
boolBranchToNix (CondBranch c  t (Just f)) = mkIf (toNix c) (boolTreeToNix t) (boolTreeToNix f)

boolTreeToNix :: (ToNixExpr v, ToNixExpr c) => CondTree v c Bool -> NExpr
boolTreeToNix (CondNode False _c _bs) = mkBool False
boolTreeToNix (CondNode True _c bs) =
  case filter (/= mkBool True) (fmap boolBranchToNix bs) of
    [] -> mkBool True
    bs' -> foldl1 ($&&) bs'

instance ToNixBinding PackageFlag where
  toNixBinding (MkPackageFlag name _desc def _manual) = (fromString . show . pretty $ name) $= mkBool def
