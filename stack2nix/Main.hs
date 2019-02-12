{-# LANGUAGE RecordWildCards   #-}
module Main where

import Nix.Pretty (prettyNix)

import Stack2nix
import Stack2nix.CLI

--------------------------------------------------------------------------------

main :: IO ()
main = print . prettyNix =<< stackexpr =<< parseStack2nixArgs
