{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Foldable (toList)
import System.Environment (getArgs)

import Data.Yaml (decodeFileEither)

import Nix.Pretty (prettyNix)
import Nix.Expr

import           Data.Aeson
import qualified Data.HashMap.Strict           as Map
import           Lens.Micro
import           Lens.Micro.Aeson

import           Cabal2Nix.Plan

main :: IO ()
main = getArgs >>= \case
  [file] -> do
    print . prettyNix =<< ltsPackages file
  _ -> putStrLn "call with /path/to/lts.yaml (Lts2Nix /path/to/lts-X.Y.yaml)"

ltsPackages :: FilePath -> IO NExpr
ltsPackages lts = do
  evalue <- decodeFileEither lts
  case evalue of
    Left  e     -> error (show e)
    Right value -> pure $ plan2nix $ lts2plan value

lts2plan :: Value -> Plan
lts2plan lts = Plan { packages , compilerVersion , compilerPackages }
 where
  packages = lts ^. key "packages" . _Object <&> \v -> Package
    { packageVersion  = v ^. key "version" . _String
    , packageRevision = v ^? key "cabal-file-info" . key "hashes" . key "SHA256" . _String
    , packageFlags    = Map.mapMaybe (^? _Bool) $ v ^. key "constraints" . key "flags" . _Object
    }
  compilerVersion = lts ^. key "system-info" . key "ghc-version" . _String
  compilerPackages =
    (lts ^.  key "system-info" . key "core-packages" . _Object <&> (Just . (^. _String)))
    <> Map.fromList
           [ (p, Nothing) -- core-executables is just a list of exe
                          -- names shipped with GHC, which lots of
                          -- packages depend on (e.g. hsc2hs)
           | p <- toList $ lts ^. key "system-info" . key "core-executables" . _Array <&> (^. _String)
           ]
