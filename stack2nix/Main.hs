{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import System.Environment (getArgs)

import Nix.Pretty (prettyNix)
import Nix.Expr

import Distribution.Types.PackageName
import Distribution.Types.PackageId
import Distribution.Compat.ReadP hiding (Parser)
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
import Data.List (isSuffixOf, isInfixOf, isPrefixOf)
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
import           Data.List.NonEmpty                       ( NonEmpty (..) )
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)

import Options.Applicative hiding (option)
import Data.Semigroup ((<>))

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status  (ok200)
import Control.Exception.Base (SomeException(..),PatternMatchFail(..))

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
-- The stack.yaml file
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- packages
--
-- * (1) Paths
--   - ./site1
--   - ./site2
-- * (2) Git Locations
--   - location:
--       git: https://github.com/yesodweb/yesod
--       commit: 7038ae6317cb3fe4853597633ba7a40804ca9a46
--     extra-dep: true
--     subdirs:
--     - yesod-core
--     - yesod-bin

--------------------------------------------------------------------------------
-- extra-deps
--
-- * (1) Package index (optional sha of cabal files contents; or revision number)
--   - acme-missiles-0.3
--   - acme-missiles-0.3@sha256:2ba66a092a32593880a87fb00f3213762d7bca65a687d45965778deb8694c5d1
--   - acme-missiles-0.3@rev:0
--
-- * (2) Local File Path (foo-1.2.3 would be parsed as a package index)
--   - vendor/somelib
--   - ./foo-1.2.3
--
-- * (3) Git and Mercurial repos (optional subdirs; or github)
--   - git: git@github.com:commercialhaskell/stack.git
--     commit: 6a86ee32e5b869a877151f74064572225e1a0398
--   - git: git@github.com:snoyberg/http-client.git
--     commit: "a5f4f3"
--   - hg: https://example.com/hg/repo
--     commit: da39a3ee5e6b4b0d3255bfef95601890afd80709
--   - git: git@github.com:yesodweb/wai
--     commit: 2f8a8e1b771829f4a8a77c0111352ce45a14c30f
--     subdirs:
--     - auto-update
--     - wai
--   - github: snoyberg/http-client
--     commit: a5f4f30f01366738f913968163d856366d7e0342
--
-- * (4) Archives (HTTP(S) or local filepath)
--   - https://example.com/foo/bar/baz-0.0.2.tar.gz
--   - archive: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
--     subdirs:
--     - wai
--     - warp
--   - archive: ../acme-missiles-0.3.tar.gz
--     sha256: e563d8b524017a06b32768c4db8eff1f822f3fb22a90320b7e414402647b735b

-- NOTE: We will only parse a suitable subset of the stack.yaml file.

--------------------------------------------------------------------------------
-- Some generic types
type Resolver = String
type Name     = String
type Compiler = String
type Sha256   = String
type CabalRev = Int    -- cabal revision 0,1,2,...
type URL      = String -- Git/Hg/... URL
type Rev      = String -- Git revision

--------------------------------------------------------------------------------
-- Data Types
-- Dependencies are the merged set of packages and extra-deps.
-- As we do not distinguish them in the same way stack does, we
-- can get away with this.
data Dependency
  = PkgIndex PackageIdentifier (Maybe (Either Sha256 CabalRev)) -- ^ overridden package in the stackage index
  | LocalPath String -- ^ Some local package (potentially overriding a package in the index as well)
  | DVCS Location [FilePath] -- ^ One or more packages fetched from git or similar.
  -- TODO: Support archives.
  -- | Archive ...
  deriving (Show)

data Stack
  = Stack Resolver (Maybe Compiler) [Dependency]
  deriving (Show)

-- stack supports custom snapshots
-- https://docs.haskellstack.org/en/stable/custom_snapshot/
data StackSnapshot
  = Snapshot
    Resolver                  -- lts-XX.YY/nightly-...
    (Maybe Compiler)          -- possible compiler override for the snapshot
    Name                      -- name
    [Dependency]              -- packages
    -- [Package -> [Flag]]    -- flags
    -- [PackageName]          -- drop-packages
    -- [PackageName -> Bool]  -- hidden
    -- [package -> [Opt]]     -- ghc-options

data Location
  = Git URL Rev
  | HG  URL Rev
  deriving (Show)


--------------------------------------------------------------------------------
-- Parsers for package indices
sha256Suffix :: ReadP r Sha256
sha256Suffix = string "@sha256:" *> many1 (satisfy (`elem` (['0'..'9']++['a'..'z']++['A'..'Z'])))

revSuffix :: ReadP r CabalRev
revSuffix = string "@rev:" *> (read <$> many1 (satisfy (`elem` ['0'..'9'])))

suffix :: ReadP r (Maybe (Either Sha256 CabalRev))
suffix = option Nothing (Just <$> (Left <$> sha256Suffix) +++ (Right <$> revSuffix))

pkgIndex :: ReadP r Dependency
pkgIndex = PkgIndex <$> parse <*> suffix <* eof

--------------------------------------------------------------------------------
-- JSON/YAML destructors

instance FromJSON Location where
  parseJSON = withObject "Location" $ \l -> Git
    <$> l .: "git"
    <*> l .: "commit"

instance FromJSON Stack where
  parseJSON = withObject "Stack" $ \s -> Stack
    <$> s .: "resolver"
    <*> s .:? "compiler" .!= Nothing
    <*> ((<>) <$> s .:? "packages"   .!= []
              <*> s .:? "extra-deps" .!= [])

instance FromJSON StackSnapshot where
  parseJSON = withObject "Snapshot" $ \s -> Snapshot
    <$> s .: "resolver"
    <*> s .:? "compiler" .!= Nothing
    <*> s .: "name"
    <*> s .:? "packages" .!= []

instance FromJSON Dependency where
  parseJSON p = parsePkgIndex p <|> parseLocalPath p <|> parseDVCS p
    where parsePkgIndex = withText "Package Index" $ \pi ->
            case [pi' | (pi',"") <- readP_to_S pkgIndex (T.unpack pi)] of
              [pi'] -> return $ pi'
              _ -> fail $ "invalid package index: " ++ show pi
          parseLocalPath = withText "Local Path" $
            return . LocalPath . dropTrailingSlash . T.unpack
          parseDVCS = withObject "DVCS" $ \o -> DVCS
            <$> (o .: "location" <|> parseJSON p)
            <*> o .:? "subdirs" .!= ["."]

          -- drop trailing slashes. Nix doesn't like them much;
          -- stack doesn't seem to care.
          dropTrailingSlash p | "/" `isSuffixOf` p = take (length p - 1) p
          dropTrailingSlash p = p

--------------------------------------------------------------------------------

main :: IO ()
main = print . prettyNix =<< stackexpr =<< execParser opts
  where opts = info (args <**> helper)
          ( fullDesc
         <> progDesc "Generate a nix expression from a stack.yaml file"
         <> header "stack-to-nix - a stack to nix converter" )

writeDoc :: FilePath -> Doc -> IO ()
writeDoc file doc =
  do handle <- openFile file WriteMode
     hPutDoc handle doc
     hClose handle

-- | A @resolver@ value in a stack.yaml file may point to an URL. As such
-- we need to be able to fetch one.
decodeURLEither :: FromJSON a => String -> IO (Either ParseException a)
decodeURLEither url
  | not (("http://" `isPrefixOf` url) || ("https://" `isPrefixOf` url))
  = return . Left . OtherParseException . SomeException . PatternMatchFail $ "No http or https prefix"
  | otherwise = do
      manager <- newManager tlsManagerSettings
      request <- parseRequest url
      response <- httpLbs request manager
      unless (ok200 == responseStatus response) $ error ("failed to download " ++ url)
      return . decodeEither' . toStrict $ responseBody response


-- | If a stack.yaml file contains a @resolver@ that points to
-- a file, resolve that file and merge the snapshot into the
-- @Stack@ record.
resolveSnapshot :: Stack -> IO Stack
resolveSnapshot stack@(Stack resolver compiler pkgs)
  = if "snapshot.yaml" `isSuffixOf` resolver
    then do evalue <- if ("http://" `isPrefixOf` resolver) || ("https://" `isPrefixOf` resolver)
                      then decodeURLEither resolver
                      else decodeFileEither resolver
            case evalue of
              Left e -> error (show e)
              Right (Snapshot resolver' compiler' _name pkgs') ->
                pure $ Stack resolver' (compiler' <|> compiler)  (pkgs <> pkgs')
    else pure stack

stackexpr :: Args -> IO NExpr
stackexpr args =
  do evalue <- decodeFileEither (stackFile args)
     case evalue of
       Left e -> error (show e)
       Right value -> stack2nix args
                      =<< resolveSnapshot value

stack2nix :: Args -> Stack -> IO NExpr
stack2nix args stack@(Stack resolver compiler _) =
  do let extraDeps = extraDeps2nix stack
     packages <- packages2nix args stack
     return . mkNonRecSet $
       [ "extra-deps" $= mkFunction "hackage" (mkNonRecSet [ "packages" $= extraDeps ])
       , "module"  $= mkFunction (mkParamset [ ("lib", Nothing) ] False)
                    (mkWith "lib" (mkNonRecSet $
                     [ "packages" $=
                      (mkSym "mapAttrs"
                       @@ ("k" ==> ("v" ==>
                        (mkSym "mkForce" @@ (mkSym "import" @@ mkSym "v"))))
                       @@ packages )]
                  -- This is a really ugly hack :(
                  -- we are injecting
                  --
                  --  compiler.version = mkForce "X.Y.Z";
                  --  compiler.nix-name = mkForce "ghcXYZ";
                  --
                  -- TODO: Do this in haskell.nix via the provided `compiler` value below.
                  ++ [ "compiler.version" $= (mkSym "mkForce" @@ fromString (quoted ver))
                     | (Just c) <- [compiler], let ver = filter (`elem` (".0123456789" :: [Char])) c]
                  ++ [ "compiler.nix-name" $= (mkSym "mkForce" @@ fromString (quoted name))
                     | (Just c) <- [compiler], let name = filter (`elem` ((['a'..'z']++['0'..'9']) :: [Char])) c]
                    ))
       , "resolver"  $= fromString (quoted resolver)
       ] ++ [
         "compiler" $= fromString (quoted c) | (Just c) <- [compiler]
       ]
-- | Transform simple package index expressions
-- The idea is to turn
--
--   - name-version[@rev:N | @sha256:SHA]
--
-- into
--
--   { name.revision = hackage.name.version.revisions.default; }
--
extraDeps2nix :: Stack -> NExpr
extraDeps2nix (Stack _ _ pkgs) =
  let extraDeps = [(pkgId, info) | PkgIndex pkgId info <- pkgs]
  in mkNonRecSet $ [ bindPath ((quoted (toText pkg)) :| ["revision"]) (mkSym "hackage" @. toText pkg @. quoted (toText ver) @. "revisions" @. "default")
                   | (PackageIdentifier pkg ver, Nothing) <- extraDeps ]
                ++ [ bindPath ((quoted (toText pkg)) :| ["revision"]) (mkSym "hackage" @. toText pkg @. quoted (toText ver) @. "revision" @. T.pack sha)
                   | (PackageIdentifier pkg ver, (Just (Left sha))) <- extraDeps ]
                ++ [ bindPath ((quoted (toText pkg)) :| ["revision"]) (mkSym "hackage" @. toText pkg @. quoted (toText ver) @. "revision" @. toText revNo)
                   | (PackageIdentifier pkg ver, (Just (Right revNo))) <- extraDeps ]
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
packages2nix args (Stack _ _ pkgs) =
  do cwd <- getCurrentDirectory
     fmap (mkNonRecSet . concat) . forM pkgs $ \case
       (LocalPath folder) ->
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
       (DVCS (Git url rev) subdirs) ->
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
