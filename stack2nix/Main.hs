{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs)

import Nix.Pretty (prettyNix)
import Nix.Expr

import Distribution.Types.PackageName
import Distribution.Types.PackageId
import Distribution.Text
import Data.Yaml
import Data.String (fromString)
import qualified Data.Text as T

import Data.Aeson
import Lens.Micro
import Lens.Micro.Aeson

import Data.Vector (toList)

import System.Directory
import System.FilePath
import Control.Monad

import Cabal2Nix hiding (Git)
import qualified Cabal2Nix as C2N
import Cabal2Nix.Util

import Text.PrettyPrint.ANSI.Leijen (hPutDoc, Doc)
import System.IO
import Data.List (isSuffixOf)
import Control.Applicative ((<|>))

import Distribution.Nixpkgs.Fetch
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

--------------------------------------------------------------------------------
-- stack.yaml parsing (subset only)
type ExtraDep = String

data Stack
  = Stack [Package] [ExtraDep]
  deriving (Show)

type URL = String
type Rev = String

data Location
  = Git URL Rev
  deriving (Show)

data Package
  = Local FilePath
  | Remote Location [FilePath]
  deriving (Show)

instance FromJSON Location where
  parseJSON = withObject "Location" $ \l -> Git
    <$> l .: "git"
    <*> l .: "commit"

instance FromJSON Package where
  parseJSON p = parseLocal p <|> parseRemote p
    where parseLocal = withText "Local Package" $ pure . Local . T.unpack
          parseRemote = withObject "Remote Package" $ \l -> Remote
            <$> l .: "location"
            <*> l .:? "subdirs" .!= ["."]

instance FromJSON Stack where
  parseJSON = withObject "Stack" $ \s -> Stack
    <$> s .: "packages"
    <*> s .: "extra-deps"
--------------------------------------------------------------------------------

writeDoc :: FilePath -> Doc -> IO ()
writeDoc file doc =
  do handle <- openFile file WriteMode
     hPutDoc handle doc
     hClose handle

main :: IO ()
main = getArgs >>= \case
  [file] -> print . prettyNix =<< stackexpr file
  _ -> putStrLn "call with stack.yaml (Stack2Nix /path/to/stack.yaml)"

stackexpr :: FilePath -> IO NExpr
stackexpr f =
  do evalue <- decodeFileEither f
     case evalue of
       Left e -> error (show e)
       Right value -> stack2nix value

stack2nix :: Stack -> IO NExpr
stack2nix stack =
  do let extraDeps = extraDeps2nix stack
     packages <- packages2nix stack
     return $ mkNonRecSet
       [ "extraDeps" $= mkFunction "hsPkgs" extraDeps
       , "packages"  $= mkFunction "hsPkgs" packages
       ]
-- | Transform 'extra-deps' to nix expressions.
-- The idea is to turn
--
--   extra-deps:
--   - name-version
--
-- into
--
--   { name = hsPkgs.name.version; }
--
extraDeps2nix :: Stack -> NExpr
extraDeps2nix (Stack _ deps) =
  let extraDeps = parsePackageIdentifier <$> deps
  in mkNonRecSet [ quoted (toText pkg) $= mkSym "hsPkgs" !. toText pkg !. toText ver
                 | Just (PackageIdentifier pkg ver) <- extraDeps ]
  where parsePackageIdentifier :: String -> Maybe PackageIdentifier
        parsePackageIdentifier = simpleParse
        toText :: Text a => a -> T.Text
        toText = fromString . show . disp

findCabalFiles :: FilePath -> IO [FilePath]
findCabalFiles =
  fmap (filter (isSuffixOf ".cabal")) . listDirectory

-- makeRelativeToCurrentDirectory
packages2nix :: Stack-> IO NExpr
packages2nix (Stack pkgs _) =
  do cwd <- getCurrentDirectory
     fmap (mkNonRecSet . concat) . forM pkgs $ \case
       (Local folder) ->
         do cabalFiles <- findCabalFiles folder
            forM cabalFiles $ \cabalFile ->
              let pkg = dropExtension . takeFileName $ cabalFile
                  nix = ".stack.nix" </> pkg <.> "nix"
                  src = Just . C2N.Path $ ".." </> folder
              in do createDirectoryIfMissing True (takeDirectory nix)
                    writeDoc nix =<<
                      prettyNix <$> cabal2nix src (folder </> cabalFile)
                    return $ fromString pkg $= mkPath False nix
       (Remote (Git url rev) subdirs) ->
         fmap concat . forM subdirs $ \subdir ->
           do fetch (\dir -> cabalFromPath url rev subdir $ dir </> subdir)
                    (Source url rev UnknownHash subdir) >>= \case
                (Just (_derivSource, bindings)) -> return bindings
                _ -> return []
       _ -> return []
  where cabalFromPath
          :: String    -- URL
          -> String    -- Revision
          -> FilePath  -- Subdir
          -> FilePath  -- Local Directory
          -> MaybeT IO [Binding NExpr]
        cabalFromPath url rev subdir path = do
          d <- liftIO $ doesDirectoryExist path
          unless d $ fail ("not a directory: " ++ path)
          cabalFiles <- liftIO $ findCabalFiles path
          forM cabalFiles $ \cabalFile ->
            let pkg = dropExtension . takeFileName $ cabalFile
                nix = ".stack.nix" </> pkg <.> "nix"
                sha256  = Nothing
                subdir' = if subdir == "." then Nothing
                          else Just subdir
                src = Just $ C2N.Git url rev sha256 subdir'
            in do liftIO $ createDirectoryIfMissing True (takeDirectory nix)
                  liftIO $ writeDoc nix =<<
                    prettyNix <$> cabal2nix src (path </> cabalFile)
                  return $ fromString pkg $= mkPath False nix

