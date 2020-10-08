module PkgA (decode) where

import Control.Lens
import Data.Text.Lens
import Data.Char
import Data.Text (Text)

decode :: Text -> Text
decode = unpacked . mapped %~ rot 13

rot :: Int -> Char -> Char
rot n c | c >= 'a' && c <= 'z' = r 'a' 'z'
        | c >= 'A' && c <= 'Z' = r 'A' 'Z'
        | otherwise            = c
  where
    r a b = chr $ ord a + ((ord c - ord a + n) `mod` (ord b - ord a + 1))
