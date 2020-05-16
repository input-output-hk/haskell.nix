{-# LANGUAGE CPP #-}
module Main where

main :: IO ()
main = print __GLASGOW_HASKELL__
