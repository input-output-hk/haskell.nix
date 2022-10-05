{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Main where

import System.Environment (getArgs)

import System.Exit
import System.Directory
import System.FilePath
import Control.Monad

import Nix.Pretty (prettyNix)
import Nix.Expr

import Data.String (fromString)

import Cabal2Nix
import Cabal2Nix.Util

import Data.Text.Prettyprint.Doc (Doc)
import Data.Text.Prettyprint.Doc.Render.Text (hPutDoc)
import System.IO
import Distribution.Nixpkgs.Fetch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.ByteString (ByteString)

import Data.List (isPrefixOf, isSuffixOf)

import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack
import qualified Data.Text as T
import qualified Data.Text.Encoding as T


writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc file doc =
  do handle <- openFile file WriteMode
     hPutDoc handle doc
     hClose handle

main :: IO ()
main = getArgs >>= \case
  [url,hash] | "http" `isPrefixOf` url ->
          let subdir = "." in
          fetch (\dir -> cabalFromPath url hash subdir $ dir </> subdir)
            (Source url mempty UnknownHash) >>= \case
            (Just (DerivationSource{..}, genBindings)) -> genBindings derivHash
            _ -> return ()
  [path,file] -> doesDirectoryExist file >>= \case
    False -> doesFileExist file >>= \case
      True -> nixFromCabal path (OnDisk file)
      False -> (findCabalFiles $ takeDirectory file) >>= \case
        [] -> die "Cabal file does not exit, no package.yaml found either"
        [cabal] -> nixFromCabal path cabal
        _ -> die "Cabal file does not exist, but multiple other cabal files found"
    True  -> print . prettyNix =<< cabalexprs file
  [file] -> doesDirectoryExist file >>= \case
    False -> nixFromCabal "." (OnDisk file)
    True  -> print . prettyNix =<< cabalexprs file
  _ -> die "call with cabalfile (Cabal2Nix file.cabal)."
  where
    nixFromCabal path cabal =
      print . prettyNix =<< cabal2nix False MinimalDetails (Just (Path path)) cabal

cabalFromPath
  :: String    -- URL
  -> String    -- Revision
  -> FilePath  -- Subdir
  -> FilePath  -- Local Directory
  -> MaybeT IO (String -> IO ())
cabalFromPath url rev subdir path = do
  d <- liftIO $ doesDirectoryExist path
  unless d $ fail ("not a directory: " ++ path)
  cabalFiles <- liftIO $ findCabalFiles path
  when (length cabalFiles > 1) $ fail ("multiple cabal files detected: " ++ show cabalFiles)
  return $ \sha256 ->
    void . forM cabalFiles $ \cabalFile -> do
      let pkg = cabalFilePkgName cabalFile
          subdir' = if subdir == "." then Nothing
                    else Just subdir
          src = Just $ Git url rev (Just sha256) subdir'
      print . prettyNix =<< cabal2nix False MinimalDetails src cabalFile

findCabalFiles :: FilePath -> IO [CabalFile]
findCabalFiles path = doesFileExist (path </> Hpack.packageConfig) >>= \case
  False -> fmap (OnDisk . (path </>)) . filter (isSuffixOf ".cabal") <$> listDirectory path
  True -> do
    mbPkg <- Hpack.readPackageConfig Hpack.defaultDecodeOptions {Hpack.decodeOptionsTarget = path </> Hpack.packageConfig}
    case mbPkg of
      Left e -> error e
      Right r ->
        return $ [InMemory (Just Hpack)
                           (Hpack.decodeResultCabalFile r)
                           (encodeUtf8 $ render r)]

  where
    render :: Hpack.DecodeResult -> String
    render r =
      let body = Hpack.renderPackage [] (Hpack.decodeResultPackage r)
          cabalVersion = Hpack.decodeResultCabalVersion r
      in cabalVersion ++ body

    encodeUtf8 :: String -> ByteString
    encodeUtf8 = T.encodeUtf8 . T.pack


expr :: FilePath -> String -> String -> IO (Binding NExpr)
expr p pkg version = do
  let cabal = OnDisk $ p </> pkg </> version </> pkg <.> "cabal"
      -- prefix packages by the truncated sha256
      -- over their name to prevent case insensitivity
      -- issues.  We truncate just to be in line with
      -- how the /nix/store path's look.
      pkg'  =       (take 32 $ sha256 pkg) ++ "-" ++ pkg
      nix   =       pkg' </> version <.> "nix"
      version' = fromString . quoted $ version
  doesFileExist (cabalFilePath cabal) >>= \case
    True ->
      do createDirectoryIfMissing True pkg'
         writeDoc nix =<< prettyNix <$> cabal2nix False MinimalDetails Nothing cabal
         pure $ version' $= mkRelPath nix
    False -> pure $ version' $= mkNull

cabalexprs :: FilePath -> IO NExpr
cabalexprs p =
  do pkgs <- listDirectories p
     fmap mkNonRecSet . forM pkgs $ \pkg ->
       do versions <- listDirectories (p </> pkg)
          let pkg' = fromString . quoted $ pkg
          fmap (bindTo pkg' . mkNonRecSet) . forM versions $ \version ->
            expr p pkg version
