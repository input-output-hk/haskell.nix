{-# LANGUAGE LambdaCase, OverloadedStrings, NamedFieldPuns, RecordWildCards #-}

module Plan2Nix
  ( doPlan2Nix
  , planexpr
  , plan2nix
  ) where

import           Data.Aeson
import           Data.Char                                ( isDigit )
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                               ( mapMaybe
                                                          , isJust
                                                          , fromMaybe
                                                          )
import           Data.List.NonEmpty                       ( NonEmpty (..) )
import qualified Data.Text                     as Text
import           Data.Text                                ( Text )
import qualified Data.Vector                   as Vector
import           Lens.Micro
import           Lens.Micro.Aeson
import           Nix.Expr
import           Nix.Pretty                               ( prettyNix )
import           System.Environment                       ( getArgs )

import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)

import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Nixpkgs.Fetch (DerivationSource(..), Source(..), Hash(..), fetch)
import Distribution.Simple.Utils (shortRelativePath)

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, forM)
import Extra (unlessM)

import Cabal2Nix hiding (Git)
import qualified Cabal2Nix as C2N
import Cabal2Nix.Util


import Plan2Nix.CLI (Args(..))
import Plan2Nix.Plan (Plan(..), PkgSrc(..), Package(..), Location(..))

import Plan2Nix.Cache (appendCache, cacheHits)
import Plan2Nix.Project

import System.FilePath ((<.>), (</>), takeDirectory, dropFileName)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.IO (IOMode(..), openFile, hClose)
import Data.String (fromString)


doPlan2Nix :: Args -> IO ()
doPlan2Nix args = do
  let pkgsNix = argOutputDir args </> "pkgs.nix"
      defaultNix = argOutputDir args </> "default.nix"
  pkgs <- planexpr args
  writeDoc pkgsNix (prettyNix pkgs)
  unlessM (doesFileExist defaultNix) $ do
    writeFile defaultNix defaultNixContents

planexpr :: Args -> IO NExpr
planexpr args =
  do evalue <- eitherDecodeFileStrict (argPlanJSON args)
     case evalue of
       Left  e     -> error (show e)
       Right value -> plan2nix args $ value2plan value

writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc file doc =
  do handle <- openFile file WriteMode
     hPutDoc handle doc
     hClose handle

plan2nix :: Args -> Plan -> IO NExpr
plan2nix args (Plan { packages, extras, compilerVersion, compilerPackages }) = do
  -- TODO: this is an aweful hack and expects plan-to-nix to be
  -- called from the toplevel project directory.
  cwd <- getCurrentDirectory
  extrasNix <- fmap (mkNonRecSet  . concat) . forM (Map.toList extras) $ \case
    (name, Just (Package v r flags (Just (LocalPath folder)))) ->
      do cabalFiles <- findCabalFiles folder
         forM cabalFiles $ \cabalFile ->
           let pkg = cabalFilePkgName cabalFile
               nix = ".plan.nix" </> pkg <.> "nix"
               nixFile = argOutputDir args </> nix
               src = Just . C2N.Path $ relPath </> ".." </> (shortRelativePath cwd folder)
           in do createDirectoryIfMissing True (takeDirectory nixFile)
                 writeDoc nixFile =<<
                   prettyNix <$> cabal2nix True (argDetailLevel args) src cabalFile
                 return $ fromString pkg $= mkPath False nix
    (name, Just (Package v r flags (Just (DVCS (Git url rev) subdirs)))) ->
      fmap concat . forM subdirs $ \subdir ->
      do cacheHits <- liftIO $ cacheHits (argCacheFile args) url rev subdir
         case cacheHits of
           [] -> do
             fetch (\dir -> cabalFromPath url rev subdir $ dir </> subdir)
               (Source url rev UnknownHash subdir) >>= \case
               (Just (DerivationSource{..}, genBindings)) -> genBindings derivHash
               _ -> return []
           hits ->
             forM hits $ \( pkg, nix ) -> do
               return $ fromString pkg $= mkPath False nix
    _ -> return []
  let flags = concatMap (\case
          (name, Just (Package _v _r f _)) -> flags2nix name f
          _ -> []) $ Map.toList extras

  return $ mkNonRecSet [
    "pkgs" $= ("hackage" ==> mkNonRecSet (
      [ "packages" $= (mkNonRecSet $ uncurry bind =<< Map.toList quotedPackages)
      , "compiler" $= mkNonRecSet
        [ "version" $= mkStr compilerVersion
        , "nix-name" $= mkStr ("ghc" <> Text.filter (/= '.') compilerVersion)
        , "packages" $= mkNonRecSet (fmap (uncurry bind') $ Map.toList $ mapKeys quoted compilerPackages)
        ]
      ]))
    , "extras" $= ("hackage" ==> mkNonRecSet [ "packages" $= extrasNix ])
    , "modules" $= mkList [
        mkParamset [("lib", Nothing)] True ==> mkNonRecSet [ "packages" $= mkNonRecSet flags ]
      ]
    ]
 where
  quotedPackages = mapKeys quoted packages
  bind pkg (Just (Package { packageVersion, packageRevision, packageFlags })) =
    let verExpr      = mkSym "hackage" @. pkg @. quoted packageVersion
        revExpr      = verExpr @. "revisions" @. maybe "default" quoted packageRevision
        flagBindings = Map.foldrWithKey
          (\fname val acc -> bindPath (pkg :| ["flags", fname]) (mkBool val) : acc)
          []
          packageFlags
    in  revBinding pkg revExpr : flagBindings
  bind pkg Nothing = [revBinding pkg mkNull]
  revBinding pkg revExpr = bindPath (pkg :| ["revision"]) revExpr
  bind' pkg ver = pkg $= maybe mkNull mkStr ver
  mapKeys f = Map.fromList . fmap (\(k, v) -> (f k, v)) . Map.toList

  relPath = shortRelativePath (argOutputDir args) (dropFileName (argCabalProject args))
  cabalFromPath
          :: String    -- URL
          -> String    -- Revision
          -> FilePath  -- Subdir
          -> FilePath  -- Local Directory
          -> MaybeT IO (String -> IO [Binding NExpr])
  cabalFromPath url rev subdir path = do
          d <- liftIO $ doesDirectoryExist path
          unless d $ fail ("not a directory: " ++ path)
          cabalFiles <- liftIO $ findCabalFiles path
          return $ \sha256 ->
            forM cabalFiles $ \cabalFile -> do
            let pkg = cabalFilePkgName cabalFile
                nix = ".plan.nix" </> pkg <.> "nix"
                nixFile = argOutputDir args </> nix
                subdir' = if subdir == "." then Nothing
                          else Just subdir
                src = Just $ C2N.Git url rev (Just sha256) subdir'
            createDirectoryIfMissing True (takeDirectory nixFile)
            writeDoc nixFile =<<
              prettyNix <$> cabal2nix True (argDetailLevel args) src cabalFile
            liftIO $ appendCache (argCacheFile args) url rev subdir sha256 pkg nix
            return $ fromString pkg $= mkPath False nix

-- | Converts the project flags for a package flags into @{ packageName = { flags = { flagA = BOOL; flagB = BOOL; }; }; }@
flags2nix :: Text -> HashMap Text Bool -> [Binding NExpr]
flags2nix pkgName pkgFlags =
  [ quoted pkgName $= mkNonRecSet
    -- `mkOverride 900` is used here so that the default values will be replaced (they are 1000).
    -- Values without a priority are treated as 100 and will replace these ones.
    [ "flags" $= mkNonRecSet [ quoted flag $= ("lib" @. "mkOverride" @@ mkInt 900 @@ mkBool val)
                             | (flag, val) <- Map.toList pkgFlags
                             ]
    ]
  ]

value2plan :: Value -> Plan
value2plan plan = Plan { packages, extras, compilerVersion, compilerPackages }
 where
  packages = fmap Just $ filterInstallPlan $ \pkg -> case ( pkg ^. key "type" . _String
                                              , pkg ^. key "style" . _String) of
    (_, "global") -> Just $ Package
      { packageVersion  = pkg ^. key "pkg-version" . _String
      , packageRevision = Nothing
      , packageFlags    = Map.mapMaybe (^? _Bool) $ pkg ^. key "flags" . _Object
      , packageSrc      = Nothing
      }
    (_, "inplace") -> Just $ Package
      { packageVersion  = pkg ^. key "pkg-version" . _String
      , packageRevision = Nothing
      , packageFlags    = Map.mapMaybe (^? _Bool) $ pkg ^. key "flags" . _Object
      , packageSrc      = Nothing
      }
    -- Until we figure out how to force Cabal to reconfigure just about any package
    -- this here might be needed, so that we get the pre-existing packages as well.
    -- Or we would have to plug in our very custom minimal pkg-db as well.
    --
    -- The issue is that cabal claims anything in the package db as pre-existing and
    -- wants to reuse it if possible.
    ("pre-existing",_) -> Just $ Package
      { packageVersion  = pkg ^. key "pkg-version" . _String
      , packageRevision = Nothing
      , packageFlags    = Map.empty
      , packageSrc      = Nothing
      }
    _ -> Nothing

  extras = fmap Just $ filterInstallPlan $ \pkg -> case ( pkg ^. key "type" . _String
                                                        , pkg ^. key "style" . _String
                                                        , pkg ^. key "pkg-src" . key "type" . _String
                                                        , pkg ^. key "pkg-src" . _Object) of
    (_, "local", "local", _) -> Just $ Package
      { packageVersion  = pkg ^. key "pkg-version" . _String
      , packageRevision = Nothing
      , packageFlags    = Map.mapMaybe (^? _Bool) $ pkg ^. key "flags" . _Object
      , packageSrc      = Just . LocalPath . Text.unpack $ pkg ^. key "pkg-src" . key "path" . _String
      }
    (_, "local", "source-repo", _) -> Just $ Package
      { packageVersion  = pkg ^. key "pkg-version" . _String
      , packageRevision = Nothing
      , packageFlags    = Map.mapMaybe (^? _Bool) $ pkg ^. key "flags" . _Object
      , packageSrc      = Just . flip DVCS [ Text.unpack $ fromMaybe "." $ pkg ^? key "pkg-src" . key "source-repo" . key "subdir" . _String ] $
          Git ( Text.unpack $ pkg ^. key "pkg-src" . key "source-repo" . key "location" . _String )
              ( Text.unpack $ pkg ^. key "pkg-src" . key "source-repo" . key "tag" . _String )
      }
    _ -> Nothing

  compilerVersion  = Text.dropWhile (not . isDigit) $ plan ^. key "compiler-id" . _String
  compilerPackages = fmap Just $ filterInstallPlan $ \pkg -> if isJust (pkg ^? key "style" . _String)
    then Nothing
    else Just $ pkg ^. key "pkg-version" . _String

  filterInstallPlan :: (Value -> Maybe b) -> HashMap Text b
  filterInstallPlan f =
    Map.fromList
      $ mapMaybe (\pkg -> (,) (pkg ^. key "pkg-name" . _String) <$> f pkg)
      $ Vector.toList (plan ^. key "install-plan" . _Array)


defaultNixContents = unlines $
  [  "{ pkgs ? import <nixpkgs> {} }:"
  , ""
  , "let"
  , "  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) { inherit pkgs; };"
  , ""
  , "  pkgSet = haskell.mkCabalProjectPkgSet {"
  , "    plan-pkgs = import ./pkgs.nix;"
  , "    pkg-def-extras = [];"
  , "    modules = [];"
  , "  };"
  , ""
  , "in"
  , "  pkgSet.config.hsPkgs"
  ]
