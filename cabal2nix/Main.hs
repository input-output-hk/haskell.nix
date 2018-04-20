{-# LANGUAGE LambdaCase #-}

module Main where

import System.Environment (getArgs)

import System.Directory
import System.FilePath
import Control.Monad

import Nix.Pretty (prettyNix)
import Nix.Expr

import Data.String (fromString)

import Cabal2Nix
import Cabal2Nix.Util

import Text.PrettyPrint.ANSI.Leijen (hPutDoc, Doc)
import System.IO

writeDoc :: FilePath -> Doc -> IO ()
writeDoc file doc =
  do handle <- openFile file WriteMode
     hPutDoc handle doc
     hClose handle

main :: IO ()
main = getArgs >>= \case
  [file] -> doesDirectoryExist file >>= \case
    False -> print . prettyNix =<< cabal2nix file
    True  -> print . prettyNix =<< cabalexprs file
  _ -> putStrLn "call with cabalfile (Cabal2Nix file.cabal)."

expr :: FilePath -> String -> String -> IO (Binding NExpr)
expr p pkg version = do
  let cabal = p </> pkg </> version </> pkg <.> "cabal"
      -- prefix packages by the truncated sha256
      -- over their name to prevent case insensitivity
      -- issues.  We truncate just to be in line with
      -- how the /nix/store path's look.
      pkg'  =       (take 32 $ sha256 pkg) ++ "-" ++ pkg
      nix   =       pkg' </> version <.> "nix"
      version' = fromString . quoted $ version
  doesFileExist cabal >>= \case
    True ->
      do createDirectoryIfMissing True pkg'
         writeDoc nix =<< prettyNix <$> cabal2nix cabal
         pure $ version' $= mkRelPath nix
    False -> pure $ version' $= mkNull

cabalexprs :: FilePath -> IO NExpr
cabalexprs p =
  do pkgs <- listDirectories p
     fmap mkNonRecSet . forM pkgs $ \pkg ->
       do versions <- listDirectories (p </> pkg)
          let pkg' = fromString . quoted $ pkg
          fmap (bindTo pkg' . mkNonRecSet) . forM versions $ \version ->
            expr p pkg version
