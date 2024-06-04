{-# LANGUAGE TemplateHaskell #-}
module Main where

import Language.Haskell.TH

main :: IO ()
main = putStrLn "Hello, Haskell!"

[d|y = 0|]
