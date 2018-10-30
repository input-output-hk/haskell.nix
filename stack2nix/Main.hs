{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import System.Environment (getArgs)

import Nix.Pretty (prettyNix)
import Nix.Expr

import Distribution.Types.PackageName
import Distribution.Types.PackageId
import Distribution.Text
import Distribution.Simple.Utils (shortRelativePath)
import Data.Yaml hiding (Parser)
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
import Data.List (isSuffixOf, isInfixOf)
import Control.Applicative ((<|>))

import Distribution.Nixpkgs.Fetch
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Exception (catch, SomeException(..))

import qualified Hpack
import qualified Hpack.Config as Hpack
import qualified Hpack.Render as Hpack
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)

import Options.Applicative
import Data.Semigroup ((<>))

--------------------------------------------------------------------------------
-- CLI Arguments
data Args = Args
  { outputPath :: FilePath
  , stackFile  :: FilePath
  } deriving Show

-- Argument Parser
args :: Parser Args
args = Args
  <$> strOption ( long "output" <> short 'o' <> metavar "DIR" <> value "." <> help "Generate output in DIR" )
  <*> argument str ( metavar "stack.yaml" )

--------------------------------------------------------------------------------
-- stack.yaml parsing (subset only)
type ExtraDep = String
type Resolver = String
type Name     = String
type Compiler = String

data Stack
  = Stack Resolver [Package] [ExtraDep]
  deriving (Show)

-- stack supports custom snapshots
-- https://docs.haskellstack.org/en/stable/custom_snapshot/
data StackSnapshot
  = Snapshot
    Resolver                  -- lts-XX.YY/nightly-...
    --(Maybe Compiler)        -- possible compiler override for the snapshot
    Name                      -- name
    [Package]                 -- packages
    -- [Package -> [Flag]]    -- flags
    -- [PackageName]          -- drop-packages
    -- [PackageName -> Bool]  -- hidden
    -- [package -> [Opt]]     -- ghc-options

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
  parseJSON p = parseLocal p <|> parseRemote p -- <|> parseRemoteInline
    where parseLocal = withText "Local Package" $ pure . Local . T.unpack
          parseRemote = withObject "Remote Package" $ \l -> Remote
            <$> (l .: "location" <|> parseJSON p)
            <*> l .:? "subdirs" .!= ["."]

instance FromJSON Stack where
  parseJSON = withObject "Stack" $ \s -> Stack
    <$> s .: "resolver"
    <*> s .:? "packages"   .!= []
    <*> s .:? "extra-deps" .!= []

instance FromJSON StackSnapshot where
  parseJSON = withObject "Snapshot" $ \s -> Snapshot
    <$> s .: "resolver"
    <*> s .: "name"
    <*> s .:? "packages" .!= []

--------------------------------------------------------------------------------

writeDoc :: FilePath -> Doc -> IO ()
writeDoc file doc =
  do handle <- openFile file WriteMode
     hPutDoc handle doc
     hClose handle

main :: IO ()
main = print . prettyNix =<< stackexpr =<< execParser opts
  where opts = info (args <**> helper)
          ( fullDesc
         <> progDesc "Generate a nix expression from a stack.yaml file"
         <> header "stack-to-nix - a stack to nix converter" )

-- | If a stack.yaml file contains a @resolver@ that points to
-- a file, resolve that file and merge the snapshot into the
-- @Stack@ record.
resolveSnapshot :: Stack -> IO Stack
resolveSnapshot stack@(Stack resolver pkgs extraPkgs)
  = if "snapshot.yaml" `isSuffixOf` resolver
    then do evalue <- decodeFileEither resolver
            case evalue of
              Left e -> error (show e)
              Right (Snapshot resolver' _name pkgs') ->
                -- Note: this is a hack.  The extra deps are
                -- either Remote or <pkg>-<version>, but we
                -- do not have logic to download remote
                -- extra deps (yet), and as such just lump
                -- them with the packages, which do have that
                -- logic.
                pure $ Stack resolver' ([p | p@(Remote{}) <- pkgs'] <> pkgs)
                                       ([p | Local p <- pkgs' ] <> extraPkgs)
    else pure stack

stackexpr :: Args -> IO NExpr
stackexpr args =
  do evalue <- decodeFileEither (stackFile args)
     case evalue of
       Left e -> error (show e)
       Right value -> stack2nix args
                      =<< cleanupStack <$> resolveSnapshot value
  where cleanupStack :: Stack -> Stack
        cleanupStack (Stack r ps es)
          = Stack r (cleanupPkg <$> (ps ++ [Local e | e <- es, looksLikePath e]))
                    (cleanupExtraDep <$> [e | e <- es, not (looksLikePath e)])
        -- drop trailing slashes. Nix doesn't like them much;
        -- stack doesn't seem to care.
        cleanupPkg (Local p) | "/" `isSuffixOf` p = Local (take (length p - 1) p)
        cleanupPkg x = x
        cleanupExtraDep = id
        looksLikePath :: String -> Bool
        looksLikePath = isInfixOf "/"

stack2nix :: Args -> Stack -> IO NExpr
stack2nix args stack@(Stack resolver _ _) =
  do let extraDeps = extraDeps2nix stack
     packages <- packages2nix args stack
     return $ mkNonRecSet
       [ "extraDeps" $= mkFunction "hsPkgs" extraDeps
       , "packages"  $= mkFunction "hsPkgs" packages
       , "resolver"  $= fromString (quoted resolver)
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
extraDeps2nix (Stack _ _ deps) =
  let extraDeps = parsePackageIdentifier <$> deps
  in mkNonRecSet [ quoted (toText pkg) $= (mkSym "hsPkgs" @. toText pkg @. quoted (toText ver))
                 | Just (PackageIdentifier pkg ver) <- extraDeps ]
  where parsePackageIdentifier :: String -> Maybe PackageIdentifier
        parsePackageIdentifier = simpleParse
        toText :: Text a => a -> T.Text
        toText = fromString . show . disp

findCabalFiles :: FilePath -> IO [CabalFile]
findCabalFiles path = doesFileExist (path </> Hpack.packageConfig) >>= \case
  False -> fmap (OnDisk . (path </>)) . filter (isSuffixOf ".cabal") <$> listDirectory path
  True -> do
    mbPkg <- Hpack.readPackageConfig Hpack.defaultDecodeOptions {Hpack.decodeOptionsTarget = path </> Hpack.packageConfig}
    case mbPkg of
      Left e -> error e
      Right r ->
        return $ [InMemory (Just Hpack)
                           (Hpack.decodeResultCabalFile r)
                           (encodeUtf8 $ Hpack.renderPackage [] (Hpack.decodeResultPackage r))]

  where encodeUtf8 :: String -> ByteString
        encodeUtf8 = T.encodeUtf8 . T.pack

readCache :: IO [( String -- url
                 , String -- rev
                 , String -- subdir
                 , String -- sha256
                 , String -- pkgname
                 , String -- nixexpr-path
                 )]
readCache = fmap (toTuple . words) . lines <$> readFile ".stack-to-nix.cache"
  where toTuple [ url, rev, subdir, sha256, pkgname, exprPath ]
          = ( url, rev, subdir, sha256, pkgname, exprPath )

appendCache :: String -> String -> String -> String -> String -> String -> IO ()
appendCache url rev subdir sha256 pkgname exprPath = do
  appendFile ".stack-to-nix.cache" $ unwords [ url, rev, subdir, sha256, pkgname, exprPath ]
  appendFile ".stack-to-nix.cache" "\n"

cacheHits :: String -> String -> String -> IO [ (String, String) ]
cacheHits url rev subdir
  = do cache <- catch' readCache (const (pure []))
       return [ ( pkgname, exprPath )
              | ( url', rev', subdir', sha256, pkgname, exprPath ) <- cache
              , url == url'
              , rev == rev'
              , subdir == subdir' ]
  where catch' :: IO a -> (SomeException -> IO a) -> IO a
        catch' = catch

-- makeRelativeToCurrentDirectory
packages2nix :: Args -> Stack-> IO NExpr
packages2nix args (Stack _ pkgs _) =
  do cwd <- getCurrentDirectory
     fmap (mkNonRecSet . concat) . forM pkgs $ \case
       (Local folder) ->
         do cabalFiles <- findCabalFiles (dropFileName (stackFile args) </> folder)
            forM cabalFiles $ \cabalFile ->
              let pkg = cabalFilePkgName cabalFile
                  nix = ".stack.nix" </> pkg <.> "nix"
                  nixFile = outputPath args </> nix
                  src = Just . C2N.Path $ relPath </> ".." </> folder
              in do createDirectoryIfMissing True (takeDirectory nixFile)
                    writeDoc nixFile =<<
                      prettyNix <$> cabal2nix src cabalFile
                    return $ fromString pkg $= mkPath False nix
       (Remote (Git url rev) subdirs) ->
         fmap concat . forM subdirs $ \subdir ->
         do cacheHits <- liftIO $ cacheHits url rev subdir
            case cacheHits of
              [] -> do
                fetch (\dir -> cabalFromPath url rev subdir $ dir </> subdir)
                  (Source url rev UnknownHash subdir) >>= \case
                  (Just (DerivationSource{..}, genBindings)) -> genBindings derivHash
                  _ -> return []
              hits ->
                forM hits $ \( pkg, nix ) -> do
                  return $ fromString pkg $= mkPath False nix
       _ -> return []
  where relPath = shortRelativePath (outputPath args) (dropFileName (stackFile args))
        cabalFromPath
          :: String    -- URL
          -> String    -- Revision
          -> FilePath  -- Subdir
          -> FilePath  -- Local Directory
          -> MaybeT IO (String -> IO [Binding NExpr])
        cabalFromPath url rev subdir path = do
          d <- liftIO $ doesDirectoryExist path
          unless d $ fail ("not a directory: " ++ path)
          cabalFiles <- liftIO $ findCabalFiles path
          return $ \sha256 ->
            forM cabalFiles $ \cabalFile -> do
            let pkg = cabalFilePkgName cabalFile
                nix = ".stack.nix" </> pkg <.> "nix"
                nixFile = outputPath args </> nix
                subdir' = if subdir == "." then Nothing
                          else Just subdir
                src = Just $ C2N.Git url rev (Just sha256) subdir'
            createDirectoryIfMissing True (takeDirectory nixFile)
            writeDoc nixFile =<<
              prettyNix <$> cabal2nix src cabalFile
            liftIO $ appendCache url rev subdir sha256 pkg nix
            return $ fromString pkg $= mkPath False nix
