{-# LANGUAGE OverloadedStrings #-}
module Cabal2Nix.Util where

import System.Directory
import System.FilePath

import Control.Monad
import Data.String (IsString)

import Data.ByteString.Char8 (pack, unpack)
import Data.Text (Text)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Base16 as Base16

import Data.List.NonEmpty (NonEmpty)
import Nix.Expr

listDirectories :: FilePath -> IO [FilePath]
listDirectories p =
  filter (/= ".git") <$> listDirectory p
  >>= filterM (doesDirectoryExist . (p </>))

quoted :: (IsString a, Semigroup a) => a -> a
quoted str = "\"" <> str <> "\""

sha256 :: String -> String
sha256 = unpack . Base16.encode . hash . pack

bindPath :: NonEmpty Text -> NExpr -> Binding NExpr
bindPath ks e = NamedVar (fmap StaticKey ks) e nullPos
