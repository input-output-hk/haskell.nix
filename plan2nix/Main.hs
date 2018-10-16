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
value2plan plan = Plan {packages , compilerVersion , compilerPackages }
 where
  packages = filterInstallPlan $ \pkg -> if (pkg ^. key "style" . _String) /= "global"
    then Nothing
    else Just $ Package
      { packageVersion  = pkg ^. key "pkg-version" . _String
      , packageRevision = Nothing
      , packageFlags    = Map.mapMaybe (^? _Bool) $ pkg ^. key "flags" . _Object
      }
  compilerVersion  = Text.dropWhile (not . isDigit) $ plan ^. key "compiler-id" . _String
  compilerPackages = filterInstallPlan $ \pkg -> if isJust (pkg ^? key "style" . _String)
    then Nothing
    else Just $ pkg ^. key "pkg-version" . _String

  filterInstallPlan :: (Value -> Maybe b) -> HashMap Text b
  filterInstallPlan f =
    Map.fromList
      $ mapMaybe (\pkg -> (,) (pkg ^. key "pkg-name" . _String) <$> f pkg)
      $ Vector.toList (plan ^. key "install-plan" . _Array)
