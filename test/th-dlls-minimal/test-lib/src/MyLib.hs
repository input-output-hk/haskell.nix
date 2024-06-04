{-# LANGUAGE ForeignFunctionInterface #-}
module MyLib (someFunc) where

import Foreign.C.Types (CInt(..))

foreign import ccall "f" c_f :: IO CInt

someFunc :: IO CInt
someFunc = do
  putStrLn "someFunc called"
  c_f
