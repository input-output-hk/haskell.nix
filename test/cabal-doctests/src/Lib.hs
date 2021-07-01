module Lib
    ( someFunc
    ) where

import Data.Aeson (encode)
import Paths_cabal_doctests_test (version)

-- |
-- >>> 1 + 1
-- 2
--
-- >>> version
-- Version {versionBranch = [0,1,0,0], versionTags = []}
--
-- >>> encode (Just 1 :: Maybe Int)
-- "1"
someFunc :: IO ()
someFunc = putStrLn "someFunc"
