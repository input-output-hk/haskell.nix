{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module Cabal2Nix (cabal2nix, cabal2nixInstWith, gpd2nix, Src(..), CabalFile(..), CabalFileGenerator(..), cabalFilePath, cabalFilePkgName, CabalDetailLevel(..)) where

import Distribution.PackageDescription.Parsec (parseGenericPackageDescription, runParseResult)
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.Verbosity (normal)
import Distribution.Pretty ( pretty, prettyShow )
import Distribution.Utils.ShortText (fromShortText)
import Distribution.Utils.Path (getSymbolicPath)
import Data.Char (toUpper)
import System.FilePath
import Data.ByteString (ByteString)
import Data.Functor ((<&>))
import Data.Maybe (catMaybes, maybeToList)
import Data.Foldable (toList)
import Distribution.Package
         ( packageName, packageVersion )
import qualified System.FilePath.Posix as FilePath.Posix
         ( joinPath, splitDirectories )
import Network.URI
         ( URI(uriAuthority, uriPath), URIAuth(..), parseURI )

import Distribution.Types.CondTree
import Distribution.Types.Library
import Distribution.Types.ForeignLib
import Distribution.PackageDescription hiding (Git)
import Distribution.Types.Version
import Distribution.Types.VersionRange
import Distribution.CabalSpecVersion
import Distribution.Compiler
import Distribution.Simple.BuildToolDepends (desugarBuildTool)
import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName

import Data.String (fromString, IsString)
import qualified Data.HashMap.Strict as Map

import Data.Maybe (mapMaybe, isJust)
import Data.Function ((&))
import Nix.Expr
import Data.Fix(Fix(..))
import Data.Text (Text)
import qualified Data.Text as Text
import Cabal2Nix.Util (quoted, selectOr)
import Cabal2Nix.Plan (InstantiatedWithMap(..), InstantiatedWith, emptyInstantiatedWithMap)

data Src
  = Path FilePath
  | Repo String (Maybe String)
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
  (OnDisk path) -> gpd2nix isLocal fileDetails src Nothing emptyInstantiatedWithMap
    <$> readGenericPackageDescription normal path
  (InMemory gen _ body) -> gpd2nix isLocal fileDetails src (genExtra <$> gen) emptyInstantiatedWithMap
    <$> case runParseResult (parseGenericPackageDescription body) of
        (_, Left (_, err)) -> error ("Failed to parse in-memory cabal file: " ++ show err)
        (_, Right desc) -> pure desc

-- | Same as 'cabal2nix' but with information about instantiated signatures.
-- See 'instantiatedWith' in 'Plan2Nix.value2plan' for details.
cabal2nixInstWith :: Bool -> CabalDetailLevel -> Maybe Src -> InstantiatedWithMap -> CabalFile -> IO NExpr
cabal2nixInstWith isLocal fileDetails src instantiatedWith = \case
  (OnDisk path) -> gpd2nix isLocal fileDetails src Nothing instantiatedWith
    <$> readGenericPackageDescription normal path
  (InMemory gen _ body) -> gpd2nix isLocal fileDetails src (genExtra <$> gen) instantiatedWith
    <$> case runParseResult (parseGenericPackageDescription body) of
        (_, Left (_, err)) -> error ("Failed to parse in-memory cabal file: " ++ show err)
        (_, Right desc) -> pure desc

gpd2nix :: Bool -> CabalDetailLevel -> Maybe Src -> Maybe NExpr -> InstantiatedWithMap -> GenericPackageDescription -> NExpr
gpd2nix isLocal fileDetails src extra instantiatedWith gpd =
  mkFunction args $ toNixGenericPackageDescription isLocal fileDetails gpd instantiatedWith
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
srcToNix pi' (Repo url mHash)
  = mkNonRecSet
    [ "src" $= applyMkDefault (mkSym pkgs @. "fetchurl" @@ mkNonRecSet
      [ "url" $= mkStr (fromString . show $ mkPrivateHackageUrl url pi')
      , "sha256" $= case mHash of
                      Nothing -> mkSym "config" @. "sha256"
                      Just hash -> mkStr (fromString hash)
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

-- | Returns the prefix of the instantiated sublib
--
-- >>> getOriginalComponentName "domain+EwFBnH3u8XWEhAhZr7XmS1"
-- "domain"
getOriginalComponentName :: Text -> Text
getOriginalComponentName = fst . Text.span (/='+')

toNixGenericPackageDescription :: Bool -> CabalDetailLevel -> GenericPackageDescription -> InstantiatedWithMap -> NExpr
toNixGenericPackageDescription isLocal detailLevel gpd (InstantiatedWithMap instantiatedWith) = mkNonRecSet
                          [ "flags"         $= (mkNonRecSet . fmap toNixBinding $ genPackageFlags gpd)
                          , "package"       $= toNixPackageDescription isLocal detailLevel (packageDescription gpd)
                          , "components"    $= components ]
    where _packageName :: IsString a => a
          _packageName = fromString . show . pretty . pkgName . package . packageDescription $ gpd

          -- Names of the sublibs with instantiated signatures
          namesOfSublibsWithInstSigs = Map.filter (not . null) instantiatedWith & Map.toList <&>
            \(pkgId, instWith) ->
              let pkgName = last (Text.split (=='-') pkgId)
              in (pkgName, instWith)

          components =
            mkNonRecSet $
              [ component "library" (lib, []) | Just lib <- [condLibrary gpd] ] ++
              (bindTo "sublibs"     . mkNonRecSet <$> filter (not . null) [ uncurry component <$> (extendTuple (condSubLibraries gpd) ++ instantiatedSublibs) ]) ++
              (bindTo "foreignlibs" . mkNonRecSet <$> filter (not . null) [ uncurry component <$> (extendTuple $ condForeignLibs  gpd) ]) ++
              (bindTo "exes"        . mkNonRecSet <$> filter (not . null) [ uncurry component <$> (extendTuple $ condExecutables  gpd) ]) ++
              (bindTo "tests"       . mkNonRecSet <$> filter (not . null) [ uncurry component <$> (extendTuple $ condTestSuites   gpd) ]) ++
              (bindTo "benchmarks"  . mkNonRecSet <$> filter (not . null) [ uncurry component <$> (extendTuple $ condBenchmarks   gpd) ])
            where
              -- We extend the tuples with instantiatedSignatures information.
              extendTuple = map (\(k, v) -> (k, (v, [])))

              -- | SubLibraries with instantiated signatures
              --
              -- We iterate over a list with sublib names with instantiation lines and extend
              -- it with a corresponding dependency tree 'CondTree' that corresponds to the original library.
              --
              -- E.g. There is a library called 'domain', and 'namesOfSublibsWithInstSigs' will have a sublib
              -- called 'domain+EwFBnH3u8XWEhAhZr7XmS1'. We take the original name without '+EwFBnH3u8XWEhAhZr7XmS1',
              -- look up a dependency tree 'CondTree' for 'domain' and return a tuple ('domain+EwFBnH3u8XWEhAhZr7XmS1', CondTree, instWith).
              instantiatedSublibs = flip mapMaybe namesOfSublibsWithInstSigs $ \(pkgName, instWith) ->
                let origPackageName = mkUnqualComponentName $ Text.unpack $ getOriginalComponentName pkgName
                    pkgName' = mkUnqualComponentName $ Text.unpack pkgName
                in lookup origPackageName (condSubLibraries gpd) <&> \condTree -> (pkgName', (condTree, instWith))

          component :: IsComponent comp => UnqualComponentName -> (CondTree ConfVar [Dependency] comp, [InstantiatedWith]) -> Binding NExpr
          component unQualName (comp, instWith)
            = quoted name $=
                mkNonRecSet (
                  [ "depends"      $= toNix (patchDeps name instWith deps) | Just deps <- [shakeTree . fmap ( (>>= depends) . targetBuildDepends . getBuildInfo) $ comp ] ] ++
                  [ "libs"         $= toNix deps | Just deps <- [shakeTree . fmap (  fmap mkSysDep . extraLibs . getBuildInfo) $ comp ] ] ++
                  [ "frameworks"   $= toNix deps | Just deps <- [shakeTree . fmap ( fmap mkSysDep . frameworks . getBuildInfo) $ comp ] ] ++
                  [ "pkgconfig"    $= toNix deps | Just deps <- [shakeTree . fmap (           pkgconfigDepends . getBuildInfo) $ comp ] ] ++
                  [ "build-tools"  $= toNix deps | Just deps <- [shakeTree . fmap (                   toolDeps . getBuildInfo) $ comp ] ] ++
                  [ "buildable"    $= boolTreeToNix (and <$> b) | Just b <- [shakeTree . fmap ((:[]) . buildable . getBuildInfo) $ comp ] ] ++
                  -- Provide component's 'instantiatedWith' line if it exists
                  [ "instantiatedWith" $= toNix (map Text.unpack instWith) | not . null $ instWith ] ++
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

          -- | Patch the dependency tree of the given component with 'InstantiatedWith' lines.
          --
          -- This function updates the dependency tree of both types of components:
          --
          -- 1. The dynamically created sublib with an instantiated signature.
          --
          -- We need to add the original package to the dependencies and the sublib with
          -- the modules that instantiates the signature.
          --
          -- For example, 'domain+EwFBnH3u8XWEhAhZr7XmS1' must have 'domain' in the 'depends' field and
          -- it must have 'impl' in the 'depends' field if the 'InstantiatedWith' line is like
          -- '<sig-name>=<pkg-name>-<version>-inplace-impl:<mod-name>'.
          --
          -- 2. The already existed component that depends on the dynamically created sublib.
          --
          -- We need to replace the original sublib with the dynamically created sublib with instantiated signature.
          --
          -- For example, 'main' depends on the 'domain' sublib which has a signature, the instantiation process
          -- creates 'domain+EwFBnH3u8XWEhAhZr7XmS1' sublib which should replace the original 'domain' in the dependency tree.
          patchDeps
            :: Text
            -> [InstantiatedWith]
            -> CondTree ConfVar [Dependency] [HaskellLibDependency]
            -> CondTree ConfVar [Dependency] [HaskellLibDependency]
          patchDeps componentName instWith CondNode{condTreeData, condTreeConstraints, condTreeComponents} =
            let
              -- [1] Creates new dependencies
              --
              -- If the given component is a dynamically created sublib,
              -- then we add the original sublib and the sublibs with instantiation modules to its dependencies.
              newSublibs = if isJust (Text.find (=='+') componentName)
                then (
                  let
                    -- We take the names of all sublibs, iterate over the list
                    -- with 'InstantiatedWith' lines and filter the sublibs that instantiated the 'componentName'.
                    namesOfAllSublibs = map (Text.pack . unUnqualComponentName . fst) $ condSubLibraries gpd
                    sublibsThatInstantiated =
                      [ subLibName
                      | instLine <- instWith
                      , let instLineWithoutModule = dropModuleName instLine
                      , subLibName <- namesOfAllSublibs
                      , Text.isSuffixOf subLibName instLineWithoutModule
                      ]

                    namesOfNewSublibs = getOriginalComponentName componentName : sublibsThatInstantiated
                  in namesOfNewSublibs <&> \sublibName ->
                    HaskellLibDependency
                      (mkPackageName _packageName)
                      (LSubLibName (mkUnqualComponentName $ Text.unpack sublibName))
                  )
                else []

              -- [2] Update existing dependencies
              --
              -- The idea here is that we need to iterate over the dependencies
              -- of the 'componentName' and replace the original's sublib name
              -- with its instantiated sublib name.
              --
              -- To find the instantiated sublib name for the given 'componentName'
              -- we need iterate over instLines, drop the module name,
              -- to find the 'InstantiatedWith' by the suffix, and return the component name
              -- that corresponds to this 'InstantiatedWith'.

              -- | This function returns the sublib names for the given component name,
              -- if this component instantiates the sublib.
              lookupSublibNameForInstantiateComponent :: Text -> [Text]
              lookupSublibNameForInstantiateComponent compName =
                map snd
                  [ (line', pkgName)
                  | (pkgName, instLines) <- namesOfSublibsWithInstSigs
                  , instLine <- instLines
                  , let line' = dropModuleName instLine
                  , Text.isSuffixOf compName line'
                  ]

              namesOfInstantiatedSublibs = flip concatMap condTreeData $ \(HaskellLibDependency _ name) -> do
                case Text.pack . unUnqualComponentName <$> libraryNameString name of
                  Just libName -> lookupSublibNameForInstantiateComponent libName
                  Nothing -> []

              originalAndInstantiatedNames = map (\s -> (getOriginalComponentName s, s)) namesOfInstantiatedSublibs
              getInstantiatedNames n = map snd $ filter ((== n) . fst) originalAndInstantiatedNames

              -- Iterate over the dependencies of 'componentName' and replace
              -- the sublib's name with a dynamically generated names.
              newCondTreeData = concat $ condTreeData <&> \(HaskellLibDependency pkgName name) ->
                let
                  newNames =
                    case name of
                      originalName@(LSubLibName (Text.pack . unUnqualComponentName -> n)) ->
                        let instantiatedNames = getInstantiatedNames n <&> \instantiatedName ->
                              -- Wrap the sublib's name with quotes because '+' in Nix can't be a field selector.
                              LSubLibName $ mkUnqualComponentName $ Text.unpack $ "\"" <> instantiatedName <> "\""
                        in if instantiatedNames == [] then [originalName] else instantiatedNames
                      LMainLibName -> [LMainLibName]
                in map (HaskellLibDependency pkgName) newNames

              dropModuleName = fst . Text.span (/=':')
            in CondNode{condTreeData = newCondTreeData ++ newSublibs, condTreeConstraints, condTreeComponents}

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
