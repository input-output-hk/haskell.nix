{-# LANGUAGE CPP #-}
module Main where

main :: IO ()
main = putStr $ show __GLASGOW_HASKELL__
