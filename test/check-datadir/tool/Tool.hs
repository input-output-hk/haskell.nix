module Main (main) where

-- A build-tool the test spawns (via `build-tool-depends`), to check the
-- v2 `check` puts build-tool exes on PATH.
main :: IO ()
main = putStrLn "tool ran"
