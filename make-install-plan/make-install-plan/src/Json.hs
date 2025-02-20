{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}

module Json (
    boolJ,
    pairJ,
    stringJ,
    listJ,
    dictJ,
    objectJ,
    nonEmptyJ,
    showJ,
    prettyShowJ,
    J.Json,
    Encoding,
    Series,
    (.=),
    (?=),
)
where

import qualified Data.List.NonEmpty as NE
import Distribution.Pretty (Pretty, prettyShow)
import qualified Distribution.Utils.Json as J

--
-- JSON utilities
--

type Encoding = J.Json
type Pair = (String, J.Json)

newtype Series = Series {runSeries :: [Pair]}
    deriving (Semigroup, Monoid) via [Pair]

class IsSeries a where
    fromSeries :: a -> [Pair]
    fromSeriesList :: [a] -> [Pair]
    fromSeriesList = concatMap fromSeries

instance IsSeries a => IsSeries [a] where
    fromSeries = fromSeriesList

instance IsSeries Series where
    fromSeries = runSeries

boolJ :: Bool -> J.Json
boolJ = J.JsonBool

listJ :: (a -> J.Json) -> [a] -> J.Json
listJ f = J.JsonArray . map f

stringJ :: String -> J.Json
stringJ = J.JsonString

dictJ :: (k -> String) -> (v -> J.Json) -> (forall a. (k -> v -> a -> a) -> a -> m -> a) -> m -> J.Json
dictJ encodeKey encodeValue foldrMapWithKey = objectJ . foldrMapWithKey go mempty
  where
    go k v c = encodeKey k .= encodeValue v <> c

(.=) :: String -> J.Json -> Series
(.=) = pairJ

(?=) :: String -> Maybe J.Json -> Series
k ?= mv = maybe mempty (k .=) mv

pairJ :: String -> J.Json -> Series
pairJ k v = Series [(k, v)]

objectJ :: IsSeries a => a -> J.Json
objectJ = J.JsonObject . fromSeries

nonEmptyJ :: (a -> J.Json) -> [a] -> Maybe J.Json
nonEmptyJ f = fmap (listJ f . NE.toList) . NE.nonEmpty

showJ :: Show a => a -> J.Json
showJ = stringJ . show

prettyShowJ :: Pretty a => a -> J.Json
prettyShowJ = stringJ . prettyShow
