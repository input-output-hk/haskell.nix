{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Cabal2Nix
import           Control.Applicative                      ( liftA2 )
import           Control.Monad.Trans.State.Strict
import           Crypto.Hash.SHA256                       ( hashlazy )
import           Data.Aeson
import           Data.Aeson.Types                         ( Pair )
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Foldable                            ( toList
                                                          , for_
                                                          )
import           Data.List                                ( intersperse )
import           Data.Map                                 ( Map )
import qualified Data.Map                      as Map
import           Data.Semigroup                as Sem
import           Data.Sequence                            ( Seq )
import qualified Data.Sequence                 as Seq
import           Data.String                              ( IsString(fromString)
                                                          )
import           Data.Text                                ( Text )
import           Data.Text.Encoding                       ( decodeUtf8 )
import           Distribution.Hackage.DB                  ( hackageTarball )
import qualified Distribution.Hackage.DB.Parsed
                                               as P
import qualified Distribution.Hackage.DB.Unparsed
                                               as U
import           Distribution.Pretty                      ( prettyShow
                                                          , Pretty
                                                          )
import           Distribution.Types.PackageName           ( PackageName )
import           Distribution.Types.Version               ( Version )
import           Nix.Pretty                               ( prettyNix )
import           System.Directory                         ( createDirectoryIfMissing
                                                          )
import           System.Environment                       ( getArgs )
import           System.FilePath                          ( (</>)
                                                          , (<.>)
                                                          )

main :: IO ()
main = do
  out:rest <- getArgs
  (inp, src) <- case rest of
                 [tarball, url] -> return (tarball, Just $ PrivateHackage url)
                 [] -> hackageTarball >>= \tarball -> return (tarball, Nothing)

  db    <- U.readTarball Nothing inp

  let (defaultJson, cabalFiles) =
        runState (fmap (object . toList . (Seq.sortOn fst)) $ foldMapWithKeyA package2json db) mempty
  createDirectoryIfMissing False out
  BL.writeFile (out </> "default.nix") $
    "with builtins; mapAttrs (_: mapAttrs (_: data: rec {\n\
     \ inherit (data) sha256;\n\
     \ revisions = (mapAttrs (rev: rdata: {\n\
     \  inherit (rdata) revNum sha256;\n\
     \  outPath = ./. + \"/hackage/${rdata.outPath}\";\n\
     \ }) data.revisions) // {\n\
     \  default = revisions.\"${data.revisions.default}\";\n\
     \ };\n\
     \})) (fromJSON (readFile ./hackage.json))\n"

  BL.writeFile (out </> "hackage.json") $ encodePretty'
    (defConfig {confCompare = compare, confIndent = Spaces 1})
    defaultJson
  createDirectoryIfMissing False (out </> "hackage")

  for_ cabalFiles $ \(cabalFile, pname, path) -> do
    gpd <- cabal2nix False MinimalDetails src $ InMemory Nothing pname $ BL.toStrict cabalFile
    writeFile (out </> path) $ show $ prettyNix gpd

type GPDWriter = State (Seq (BL.ByteString, String, FilePath))

newtype ApplicativeMonoid f a = ApplicativeMonoid { unApplicativeMonoid :: f a }
instance (Applicative f, Semigroup a) => Sem.Semigroup (ApplicativeMonoid f a) where
  ApplicativeMonoid a <> ApplicativeMonoid b = ApplicativeMonoid $ liftA2 (Sem.<>) a b
instance (Applicative f, Monoid a) => Monoid (ApplicativeMonoid f a) where
  mempty = ApplicativeMonoid $ pure mempty
  mappend = (Sem.<>)

foldMapWithKeyA
  :: (Applicative f, Monoid b) => (k -> a -> f b) -> Map k a -> f b
foldMapWithKeyA f =
  unApplicativeMonoid . Map.foldMapWithKey (\k -> ApplicativeMonoid . f k)

fromPretty :: (Pretty a, IsString b) => a -> b
fromPretty = fromString . prettyShow

package2json :: PackageName -> U.PackageData -> GPDWriter (Seq Pair)
package2json pname (U.PackageData { U.versions }) = do
  versionBindings <- foldMapWithKeyA (version2json pname) versions
  return $ Seq.singleton $ fromPretty pname .= (object . toList $ Seq.sortOn fst $ versionBindings)

version2json
  :: PackageName -> Version -> U.VersionData -> GPDWriter (Seq (Pair))
version2json pname vnum (U.VersionData { U.cabalFileRevisions, U.metaFile }) =
  do
    revisionBindings <- sequenceA
      $ zipWith (revBindingJson pname vnum) cabalFileRevisions [0 ..]
    let hash = decodeUtf8 $ fromString $ P.parseMetaData pname vnum metaFile Map.! "sha256"
    return $ Seq.singleton $ fromPretty vnum .= object
      [ "sha256" .= hash
      , "revisions" .= object
        ( revisionBindings
        ++ ["default" .= fst (last revisionBindings)]
        )
      ]

revBindingJson
  :: PackageName
  -> Version
  -> BL.ByteString
  -> Integer
  -> GPDWriter (Text, Value)
revBindingJson pname vnum cabalFile revNum = do
  let qualifiedName = mconcat $ intersperse
        "-"
        [prettyPname, fromPretty vnum, revName, BS.unpack cabalHash]
      revName :: (Semigroup a, IsString a) => a
      revName     = "r" <> fromString (show revNum)
      revPath     = "." </> "hackage" </> qualifiedName <.> "nix"
      prettyPname = fromPretty pname
      cabalHash   = Base16.encode $ hashlazy cabalFile
  modify' $ mappend $ Seq.singleton
    (cabalFile, prettyPname ++ ".cabal", revPath)
  return $ revName .= object
    [ "outPath" .= (qualifiedName <> ".nix")
    , "revNum" .= revNum
    , "sha256" .= decodeUtf8 cabalHash
    ]
