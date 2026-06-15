module Main (main) where

import Control.Monad (unless)
import Data.List (isInfixOf)
import Paths_check_datadir (getDataFileName)
import System.Exit (exitFailure)
import System.Process (readProcess)

-- Exercises the parts of a `cabal v2-test` environment that the
-- builderVersion = 2 `check` (which runs the installed test binary directly)
-- must reproduce — see lib/check.nix:
--
--   1. `data-files` via `getDataFileName` — needs `<pkg>_datadir`.
--   2. a source-relative file — needs the check to run in the package source.
--   3. a `build-tool-depends` exe — needs it on `PATH`.
--
-- Each fails with "does not exist" if the corresponding piece is missing.
main :: IO ()
main = do
  dataPath <- getDataFileName "data-file.txt"
  dataContents <- readFile dataPath
  expect "data-files (getDataFileName)" ("data-files work" `isInfixOf` dataContents)

  golden <- readFile "golden.txt"
  expect "source-relative file" ("golden ok" `isInfixOf` golden)

  toolOut <- readProcess "check-datadir-tool" [] ""
  expect "build-tool on PATH" ("tool ran" `isInfixOf` toolOut)

  putStrLn "all check-datadir checks OK"
  where
    expect name ok =
      unless ok $ do
        putStrLn ("FAILED: " ++ name)
        exitFailure
