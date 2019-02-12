{-# LANGUAGE LambdaCase #-}

module Stack2nix.Project
  ( findCabalFiles
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)

import System.FilePath ((</>))
import System.Directory (listDirectory, doesFileExist)
import Data.List (isSuffixOf)

import qualified Hpack
import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack

import Cabal2Nix (CabalFile(..), CabalFileGenerator(..))

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
                           (encodeUtf8 $ Hpack.renderPackage [] (Hpack.decodeResultPackage r))]

  where encodeUtf8 :: String -> ByteString
        encodeUtf8 = T.encodeUtf8 . T.pack
