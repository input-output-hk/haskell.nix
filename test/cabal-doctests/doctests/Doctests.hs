module Main where

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import System.Environment (unsetEnv)
import Test.DocTest (doctest)

main :: IO ()
main = do
  putStrLn ""
  traverse_ putStrLn args -- optionally print arguments
  unsetEnv "GHC_ENVIRONMENT" -- see 'Notes'; you may not need this
  doctest args
  where
    args :: [String]
    args = flags ++ pkgs ++ module_sources -- ++ ["-v"]
