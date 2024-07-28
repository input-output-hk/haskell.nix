module Main where

import GHC.Types.SrcLoc

main :: IO ()
main = putStrLn "yeah"

data Foo = Foo PsLoc

