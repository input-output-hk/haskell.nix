{-# LANGUAGE CPP #-}
module Lib
    ( someFunc
    ) where

#ifdef TEST_GHC_OPTION
someFunc :: IO ()
someFunc = putStrLn "someFunc"
#endif
