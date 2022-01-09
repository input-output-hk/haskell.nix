{-# LANGUAGE ForeignFunctionInterface #-}
module Lib where

foreign import ccall "add1" c_add1 :: Double -> Double


