{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)

import Data.Yaml (decodeFileEither)

import Nix.Pretty (prettyNix)
import Nix.Expr

import Data.Aeson
import Lens.Micro
import Lens.Micro.Aeson

import Data.HashMap.Strict (toList)

import Cabal2Nix.Util
import Data.Text as T (filter)

main :: IO ()
main = getArgs >>= \case
  [file] -> do
    print . prettyNix =<< ltsPackages file
  _ -> putStrLn "call with /path/to/lts.yaml (Lts2Nix /path/to/lts-X.Y.yaml)"

ltsPackages :: FilePath -> IO NExpr
ltsPackages lts = do
  evalue <- decodeFileEither lts
  case evalue of
    Left e -> error (show e)
    Right value -> pure $ lts2nix value

lts2nix :: Value -> NExpr
lts2nix lts = mkFunction "hackage" . mkNonRecSet $
  [ "packages"          $= (mkNonRecSet $ uncurry bind  <$> toList pkgs)
  , "compiler" $= mkNonRecSet
    [ "version"  $= mkStr (lts ^. key "system-info" . key "ghc-version" . _String)
    , "nix-name" $= mkStr ("ghc" <> (T.filter (/= '.') $ lts ^. key "system-info" . key "ghc-version" . _String))
    , "packages" $= (mkNonRecSet $ uncurry bind' <$> toList corePkgs)
    ]
  ]
  where pkgs = lts ^. key "packages" . _Object
              <&> (^. key "version" . _String)
        corePkgs = lts ^. key "system-info" . key "core-packages" . _Object
                   <&> (^. _String)
        bind pkg ver = quoted pkg $= mkSym "hackage" !. pkg !. ver
        bind' pkg ver = quoted pkg $= mkStr ver

