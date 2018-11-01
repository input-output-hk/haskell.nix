{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main
where

import           Cabal2Nix.Plan
import           Data.Aeson
import           Data.Char                                ( isDigit )
import           Data.HashMap.Strict                      ( HashMap )
import qualified Data.HashMap.Strict           as Map
import           Data.Maybe                               ( mapMaybe
                                                          , isJust
                                                          )
import qualified Data.Text                     as Text
import           Data.Text                                ( Text )
import qualified Data.Vector                   as Vector
import           Lens.Micro
import           Lens.Micro.Aeson
import           Nix.Expr                                 ( NExpr )
import           Nix.Pretty                               ( prettyNix )
import           System.Environment                       ( getArgs )

main :: IO ()
main = getArgs >>= \case
  [planJSON] -> do
    print . prettyNix =<< planPackages planJSON
  _ -> putStrLn "call with /path/to/plan.json (Plan2Nix /path/to/plan.json)"

planPackages :: FilePath -> IO NExpr
planPackages planJSON = do
  evalue <- eitherDecodeFileStrict planJSON
  case evalue of
    Left  e     -> error (show e)
    Right value -> pure $ plan2nix $ value2plan value

value2plan :: Value -> Plan
value2plan plan = Plan { packages, compilerVersion, compilerPackages }
 where
  packages = fmap Just $ filterInstallPlan $ \pkg -> case ( pkg ^. key "type" . _String
                                              , pkg ^. key "style" . _String) of
    (_, "global") -> Just $ Package
      { packageVersion  = pkg ^. key "pkg-version" . _String
      , packageRevision = Nothing
      , packageFlags    = Map.mapMaybe (^? _Bool) $ pkg ^. key "flags" . _Object
      }
    -- Until we figure out how to force Cabal to reconfigure just about any package
    -- this here might be needed, so that we get the pre-existing packages as well.
    -- Or we would have to plug in our very custom minimal pkg-db as well.
    --
    -- The issue is that cabal claims anything in the package db as pre-existing and
    -- wants to reuse it if possible.
    ("pre-existing",_) -> Just $ Package
      { packageVersion  = pkg ^. key "pkg-version" . _String
      , packageRevision = Nothing
      , packageFlags    = Map.empty
      }
    _ -> Nothing
  compilerVersion  = Text.dropWhile (not . isDigit) $ plan ^. key "compiler-id" . _String
  compilerPackages = fmap Just $ filterInstallPlan $ \pkg -> if isJust (pkg ^? key "style" . _String)
    then Nothing
    else Just $ pkg ^. key "pkg-version" . _String

  filterInstallPlan :: (Value -> Maybe b) -> HashMap Text b
  filterInstallPlan f =
    Map.fromList
      $ mapMaybe (\pkg -> (,) (pkg ^. key "pkg-name" . _String) <$> f pkg)
      $ Vector.toList (plan ^. key "install-plan" . _Array)
