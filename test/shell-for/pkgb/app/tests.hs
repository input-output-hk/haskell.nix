module Main where

import System.Process

main :: IO ()
main = callProcess "pkga-exe" []
