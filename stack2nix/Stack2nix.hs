{-# LANGUAGE LambdaCase, RecordWildCards, OverloadedStrings #-}

module Stack2nix
  ( doStack2nix
  , stackexpr
  , stack2nix
  ) where

import qualified Data.Text as T
import Data.String (fromString)

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, forM)
import Extra (unlessM)

import System.FilePath ((<.>), (</>), takeDirectory, dropFileName)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import System.IO (IOMode(..), openFile, hClose)
import Data.Yaml (decodeFileEither)

import Nix.Expr
import Nix.Pretty (prettyNix)
import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)

import Distribution.Types.PackageId (PackageIdentifier(..))
import Distribution.Nixpkgs.Fetch (DerivationSource(..), Source(..), Hash(..), fetch)
import Distribution.Simple.Utils (shortRelativePath)
import Distribution.Text (Text(..), simpleParse)

import Cabal2Nix hiding (Git)
import qualified Cabal2Nix as C2N
import Cabal2Nix.Util

import Stack2nix.Cache (appendCache, cacheHits)
import Stack2nix.CLI (Args(..))
import Stack2nix.Project
import Stack2nix.Stack (Stack(..), Dependency(..), Location(..), PackageFlags, GhcOptions)
import Stack2nix.External.Resolve

import qualified Data.HashMap.Strict as HM


doStack2nix :: Args -> IO ()
doStack2nix args = do
  let pkgsNix = argOutputDir args </> "pkgs.nix"
      defaultNix = argOutputDir args </> "default.nix"
  pkgs <- stackexpr args
  writeDoc pkgsNix (prettyNix pkgs)
  unlessM (doesFileExist defaultNix) $ do
    writeFile defaultNix defaultNixContents

stackexpr :: Args -> IO NExpr
stackexpr args =
  do evalue <- decodeFileEither (argStackYaml args)
     case evalue of
       Left e -> error (show e)
       Right value -> stack2nix args
                      =<< resolveSnapshot value

stack2nix :: Args -> Stack -> IO NExpr
stack2nix args stack@(Stack resolver compiler pkgs pkgFlags ghcOpts) =
  do let extraDeps    = extraDeps2nix pkgs
         flags        = flags2nix pkgFlags
         ghcOptions   = ghcOptions2nix ghcOpts
     let _f_          = mkSym "f"
         _import_     = mkSym "import"
         _mkForce_    = mkSym "mkForce"
         _isFunction_ = mkSym "isFunction"
         _mapAttrs_   = mkSym "mapAttrs"
         _config_     = mkSym "config"
     packages <- packages2nix args pkgs
     return . mkNonRecSet $
       [ "extras" $= ("hackage" ==> mkNonRecSet
                     ([ "packages" $= mkNonRecSet (extraDeps <> packages) ]
                   ++ [ "compiler.version" $= fromString (quoted ver)
                      | (Just c) <- [compiler], let ver = filter (`elem` (".0123456789" :: [Char])) c]
                   ++ [ "compiler.nix-name" $= fromString (quoted name)
                      | (Just c) <- [compiler], let name = filter (`elem` ((['a'..'z']++['0'..'9']) :: [Char])) c]))
       , "resolver"  $= fromString (quoted resolver)
       , "modules" $= mkList [
           mkParamset [("lib", Nothing)] True ==> mkNonRecSet [ "packages" $= mkNonRecSet flags ]
         , mkNonRecSet [ "packages" $= mkNonRecSet ghcOptions ] ]
       ] ++ [
         "compiler" $= fromString (quoted c) | (Just c) <- [compiler]
       ]
-- | Transform simple package index expressions
-- The idea is to turn
--
--   - name-version[@rev:N | @sha256:SHA]
--
-- into
--
--   { name.revision = hackage.name.version.revisions.default; }
--
extraDeps2nix :: [Dependency] -> [Binding NExpr]
extraDeps2nix pkgs =
  let extraDeps = [(pkgId, info) | PkgIndex pkgId info <- pkgs]
  in [ (quoted (toText pkg)) $= (mkSym "hackage" @. toText pkg @. quoted (toText ver) @. "revisions" @. "default")
     | (PackageIdentifier pkg ver, Nothing) <- extraDeps ]
  ++ [ (quoted (toText pkg)) $= (mkSym "hackage" @. toText pkg @. quoted (toText ver) @. "revisions" @. quoted (T.pack sha))
     | (PackageIdentifier pkg ver, (Just (Left sha))) <- extraDeps ]
  ++ [ (quoted (toText pkg)) $= (mkSym "hackage" @. toText pkg @. quoted (toText ver) @. "revisions" @. toText revNo)
     | (PackageIdentifier pkg ver, (Just (Right revNo))) <- extraDeps ]
  where parsePackageIdentifier :: String -> Maybe PackageIdentifier
        parsePackageIdentifier = simpleParse
        toText :: Text a => a -> T.Text
        toText = fromString . show . disp

-- | Converts 'PackageFlags' into @{ packageName = { flags = { flagA = BOOL; flagB = BOOL; }; }; }@
flags2nix :: PackageFlags -> [Binding NExpr]
flags2nix pkgFlags =
  [ quoted pkgName $= mkNonRecSet
    -- `mkOverride 900` is used here so that the default values will be replaced (they are 1000).
    -- Values without a priority are treated as 100 and will replace these ones.
    [ "flags" $= mkNonRecSet [ quoted flag $= ("lib" @. "mkOverride" @@ mkInt 900 @@ mkBool val)
                             | (flag, val) <- HM.toList flags
                             ]
    ]
  | (pkgName, flags) <- HM.toList pkgFlags
  ]

-- | Converts 'GhcOptions' into @{ packageName = { ghcOptions = "..."; }; }@
ghcOptions2nix :: GhcOptions -> [Binding NExpr]
ghcOptions2nix ghcOptions =
  [ quoted pkgName $= mkNonRecSet
    [ "package" $= mkNonRecSet [ "ghcOptions" $= mkStr opts ] ]
  | (pkgName, opts) <- HM.toList ghcOptions
  ]

writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc file doc =
  do handle <- openFile file WriteMode
     hPutDoc handle doc
     hClose handle


-- makeRelativeToCurrentDirectory
packages2nix :: Args -> [Dependency] -> IO [Binding NExpr]
packages2nix args pkgs =
  do cwd <- getCurrentDirectory
     fmap concat . forM pkgs $ \case
       (LocalPath folder) ->
         do cabalFiles <- findCabalFiles (argHpackUse args) (dropFileName (argStackYaml args) </> folder)
            forM cabalFiles $ \cabalFile ->
              let pkg = cabalFilePkgName cabalFile
                  nix = pkg <.> "nix"
                  nixFile = argOutputDir args </> nix
                  src = Just . C2N.Path $ relPath </> folder
              in do createDirectoryIfMissing True (takeDirectory nixFile)
                    writeDoc nixFile =<<
                      prettyNix <$> cabal2nix True (argDetailLevel args) src cabalFile
                    return $ fromString pkg $= mkPath False nix
       (DVCS (Git url rev) subdirs) ->
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
  where relPath = shortRelativePath (argOutputDir args) (dropFileName (argStackYaml args))
        cabalFromPath
          :: String    -- URL
          -> String    -- Revision
          -> FilePath  -- Subdir
          -> FilePath  -- Local Directory
          -> MaybeT IO (String -> IO [Binding NExpr])
        cabalFromPath url rev subdir path = do
          d <- liftIO $ doesDirectoryExist path
          unless d $ fail ("not a directory: " ++ path)
          cabalFiles <- liftIO $ findCabalFiles (argHpackUse args) path
          return $ \sha256 ->
            forM cabalFiles $ \cabalFile -> do
            let pkg = cabalFilePkgName cabalFile
                nix = pkg <.> "nix"
                nixFile = argOutputDir args </> nix
                subdir' = if subdir == "." then Nothing
                          else Just subdir
                src = Just $ C2N.Git url rev (Just sha256) subdir'
            createDirectoryIfMissing True (takeDirectory nixFile)
            writeDoc nixFile =<<
              prettyNix <$> cabal2nix True (argDetailLevel args) src cabalFile
            liftIO $ appendCache (argCacheFile args) url rev subdir sha256 pkg nix
            return $ fromString pkg $= mkPath False nix

defaultNixContents :: String
defaultNixContents = unlines
  [  "{ pkgs ? import <nixpkgs> {} }:"
  , ""
  , "let"
  , "  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) { inherit pkgs; };"
  , ""
  , "  pkgSet = haskell.mkStackPkgSet {"
  , "    stack-pkgs = import ./pkgs.nix;"
  , "    pkg-def-extras = [];"
  , "    modules = [];"
  , "  };"
  , ""
  , "in"
  , "  pkgSet.config.hsPkgs"
  ]
