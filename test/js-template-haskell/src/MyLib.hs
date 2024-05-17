{-# LANGUAGE QuasiQuotes #-}

module MyLib (someUri) where

import URI.ByteString.QQ

someUri :: String
someUri = show [uri|https://www.example.com/|]

