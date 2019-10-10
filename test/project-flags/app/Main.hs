{-# LANGUAGE CPP #-}
module Main where

import Lib

#ifdef TEST_FLAG
main :: IO ()
main = someFunc
#endif
