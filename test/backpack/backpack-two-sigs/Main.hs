module Main where

import qualified ImplA
import qualified B
import qualified ImplC

main :: IO ()
main = do
  B.hello (ImplA.SomeData "hello")
  B.hello2 (ImplC.SomeData 3)
