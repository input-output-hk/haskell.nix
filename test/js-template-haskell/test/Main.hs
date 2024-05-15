module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)

import MyLib (someUri)

expected, actual :: String
expected = "URI {uriScheme = Scheme {schemeBS = \"https\"}, uriAuthority = Just (Authority {authorityUserInfo = Nothing, authorityHost = Host {hostBS = \"www.example.com\"}, authorityPort = Nothing}), uriPath = \"/\", uriQuery = Query {queryPairs = []}, uriFragment = Nothing}"
actual = someUri

main :: IO ()
main =
  unless (expected == actual) $ do
    putStrLn $ "Unexpected TH result : " <> actual
    exitFailure

