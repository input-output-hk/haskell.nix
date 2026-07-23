{-# LANGUAGE OverloadedStrings #-}
module Cabal2Nix.Util where

import System.Directory
import System.FilePath

import Control.Monad
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

import Data.ByteString.Char8 (pack, unpack)
import Crypto.Hash.SHA256 (hash)
import qualified Data.ByteString.Base16 as Base16

import Data.List.NonEmpty (NonEmpty)
import Data.Fix(Fix(..))
import Nix.Expr

listDirectories :: FilePath -> IO [FilePath]
listDirectories p =
  filter (/= ".git") <$> listDirectory p
  >>= filterM (doesDirectoryExist . (p </>))

quoted :: (IsString a, Semigroup a) => a -> a
quoted str = "\"" <> str <> "\""

selectOr :: NExpr -> NAttrPath NExpr -> NExpr -> NExpr
selectOr obj path alt = Fix (NSelect (Just $ alt) obj path)

mkThrow :: NExpr -> NExpr
mkThrow msg = (mkSym "builtins" @. "throw") @@ msg

-- | Build a reference to @hackage.<pkg>.<ver>@ using the two given attribute
-- keys verbatim (so callers keep whatever quoting they already use), but guard
-- both lookups so that a package or version missing from hackage.nix throws an
-- actionable error instead of Nix's opaque @attribute '<ver>' missing@ (#855).
-- The surrounding quotes on the keys are stripped for the human-readable message
-- only; the generated selectors are identical to the previous @hackage.<pkg>.<ver>@.
hackageVersion :: Text -> Text -> NExpr
hackageVersion pkgKey verKey =
  selectOr
    (selectOr (mkSym "hackage") (mkSelector pkgKey)
      (mkThrow (mkStr (unq pkgKey <> notInHackage))))
    (mkSelector verKey)
    (mkThrow (mkStr (unq pkgKey <> "-" <> unq verKey <> notInHackage)))
  where
    unq = T.dropAround (== '"')
    notInHackage =
      " is not in the hackage.nix package set. You may need a more recent\
      \ hackage.nix (update your haskell.nix pin / niv sources / flake inputs),\
      \ or your index-state may be too old to include it."

sha256 :: String -> String
sha256 = unpack . Base16.encode . hash . pack

bindPath :: NonEmpty VarName -> NExpr -> Binding NExpr
bindPath ks e = NamedVar (fmap StaticKey ks) e nullPos
