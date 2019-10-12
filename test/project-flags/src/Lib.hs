{-# LANGUAGE CPP #-}
module Lib
    ( someFunc
    ) where

#ifdef TEST_FLAG
someFunc :: IO ()
someFunc = putStrLn "someFunc"
#endif
