{-# LANGUAGE OverloadedStrings #-}

-- | Pure parser tests for the stack @resolver@/@snapshot@ field (#1677).
-- Self-contained: no external tools or network, so this suite is safe to run
-- in CI without the golden-test fixtures.
module Main (main) where

import qualified Data.ByteString.Char8 as B8
import qualified Data.Yaml as Y
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, assertFailure, (@?=))

import Stack2nix.Stack (Stack(..), StackSnapshot(..))

-- Decode a YAML document, failing the test with the parse error otherwise.
decode :: Y.FromJSON a => String -> IO a
decode s = case Y.decodeEither' (B8.pack s) of
  Left e  -> assertFailure ("unexpected parse failure: " ++ show e) >> undefined
  Right a -> pure a

stackResolver :: Stack -> String
stackResolver (Stack r _ _ _ _) = r

snapshotResolver :: StackSnapshot -> String
snapshotResolver (Snapshot r _ _ _ _ _) = r

main :: IO ()
main = defaultMain $ testGroup "stack resolver parsing"
  [ testCase "stack.yaml resolver as a plain string" $ do
      s <- decode "resolver: lts-20.11\n"
      stackResolver s @?= "lts-20.11"

  , testCase "stack.yaml snapshot synonym" $ do
      s <- decode "snapshot: lts-21.0\npackages: []\n"
      stackResolver s @?= "lts-21.0"

  , testCase "stack.yaml resolver in pantry object form uses url (#1677)" $ do
      s <- decode "resolver:\n  url: https://example.com/snap.yaml\n  sha256: deadbeef\n  size: 42\n"
      stackResolver s @?= "https://example.com/snap.yaml"

  , testCase "custom snapshot resolver as a plain string" $ do
      s <- decode "name: my-snap\nresolver: lts-20.11\npackages: []\n"
      snapshotResolver s @?= "lts-20.11"

  , testCase "custom snapshot resolver in pantry object form (#1677)" $ do
      s <- decode "name: my-snap\nresolver:\n  url: https://example.com/snap.yaml\n  sha256: deadbeef\n  size: 42\npackages: []\n"
      snapshotResolver s @?= "https://example.com/snap.yaml"

  , testCase "resolver object without a url is rejected" $
      case Y.decodeEither' (B8.pack "resolver:\n  sha256: deadbeef\n") :: Either Y.ParseException Stack of
        Left _  -> pure ()
        Right s -> assertFailure ("expected failure, got resolver " ++ show (stackResolver s))
  ]
