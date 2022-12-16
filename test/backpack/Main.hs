module Main where

import qualified ImplA
import qualified B

main :: IO ()
main = B.hello (ImplA.SomeData "hello")
