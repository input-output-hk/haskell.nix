{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (unless)
import qualified Data.Text as T (pack)
import Data.Text.Encoding (encodeUtf8)
import System.Exit (exitFailure)
import Data.FileEmbed (embedFile, makeRelativeToProject)

main :: IO ()
main =
  unless ($(makeRelativeToProject "app/test.txt" >>= embedFile) == encodeUtf8 (T.pack "Hello World")) exitFailure
