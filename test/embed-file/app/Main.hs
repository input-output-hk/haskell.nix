{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (unless)
import qualified Data.Text as T (pack)
import Data.Text.Encoding (encodeUtf8)
import System.Exit (exitFailure)
import Data.FileEmbed (embedFile, makeRelativeToProject)

main :: IO ()
main = do
  let test = $(makeRelativeToProject "app/test.txt" >>= embedFile)
  unless (test == encodeUtf8 (T.pack "Hello World\n")) $ do
    putStrLn $ "Embedded content was : " <> show test
    exitFailure
