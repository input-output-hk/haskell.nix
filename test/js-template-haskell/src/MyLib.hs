{-# LANGUAGE QuasiQuotes #-}

module MyLib (someFunc) where

import URI.ByteString.QQ

someFunc :: IO ()
someFunc = putStrLn $ show [uri|https://www.example.com/|]
