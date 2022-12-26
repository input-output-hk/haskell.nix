module Main where

import qualified TestImplA
import qualified B

main :: IO ()
main = B.hello (TestImplA.SomeData 3)
