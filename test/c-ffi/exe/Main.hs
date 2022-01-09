module Main where

import Control.Monad (when)
import System.Exit (exitFailure)

import Lib (c_add1)

main :: IO ()
main =
  if c_add1 1 /= 2
    then do
      putStrLn "Unexpected Result calling `c_add1 1`"
      exitFailure
    else putStrLn "OK"

