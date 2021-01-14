module Main where

import ConduitExample (example)
import PkgB (message)
import qualified Data.Text.IO as T

main :: IO ()
main = do
  T.putStrLn message
  example
