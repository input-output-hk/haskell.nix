{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens

data Point = Point { _x :: Double, _y :: Double }
makeLenses ''Point

main :: IO ()
main = print (point^.x + point^.y)
  where
    point = Point { _x = 40.0, _y = 2.0 }
