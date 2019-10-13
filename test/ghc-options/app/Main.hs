{-# LANGUAGE CPP #-}
module Main where

import Lib

#ifdef TEST_GHC_OPTION
main :: IO ()
main = someFunc
#endif
