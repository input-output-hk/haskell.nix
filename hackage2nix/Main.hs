{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main
where

import           Cabal2Nix
import           Cabal2Nix.Util                           ( quoted )
import           Crypto.Hash.SHA256                       ( hash
                                                          , hashlazy
                                                          )
import qualified Data.ByteString.Base16        as Base16
import qualified Data.ByteString.Char8         as BS
import qualified Data.ByteString.Lazy          as BL
import           Data.Foldable                            ( toList )
import qualified Data.Map                      as Map
import qualified Data.Sequence                 as Seq
import           Data.String                              ( IsString(fromString) )
import           Data.Text.Encoding                       ( decodeUtf8 )
import           Distribution.Hackage.DB                  ( hackageTarball )
import qualified Distribution.Hackage.DB.Parsed
                                               as P
import           Distribution.Hackage.DB.Parsed           ( parseMetaData
                                                          , parseVersionData
                                                          )
import           Distribution.Hackage.DB.Unparsed
import           Distribution.Pretty                      ( prettyShow
                                                          , Pretty
                                                          )
import           Nix.Expr
import           Nix.Pretty                               ( prettyNix )
import           System.Directory                         ( createDirectoryIfMissing )
import           System.Environment                       ( getArgs )
import           System.FilePath                          ( (</>)
                                                          , (<.>)
                                                          )

main :: IO ()
main = do
  [out] <- getArgs
  db    <- readTarball Nothing =<< hackageTarball

  let defaultNix = seqToSet $ Map.foldMapWithKey package2nix db
  createDirectoryIfMissing False out
  writeFile (out </> "default.nix") $ show $ prettyNix defaultNix

  _ <- forWithKey db $ \pname (PackageData { versions }) ->
    forWithKey versions $ \vnum vdata@(VersionData { cabalFileRevisions }) ->
      let parsedVData = parseVersionData pname vnum vdata
          writeFiles gpd cabalFile revNum = do
            let dir     = out </> packagePath pname </> fromPretty vnum
                revPath = dir </> revName revNum
            createDirectoryIfMissing True dir
            BL.writeFile (revPath <.> "cabal") cabalFile
            writeFile (revPath <.> "nix") $ show $ prettyNix $ gpd2nix Nothing Nothing gpd
      in  sequence $ zipWith3 writeFiles
                              (toList (P.cabalFileRevisions parsedVData))
                              cabalFileRevisions
                              [(0 :: Int) ..]
  return ()
 where
  forWithKey :: Applicative f => Map.Map k v -> (k -> v -> f x) -> f (Map.Map k x)
  forWithKey = flip Map.traverseWithKey
  seqToSet   = mkNonRecSet . toList
  fromPretty :: (Pretty a, IsString b) => a -> b
  fromPretty = fromString . prettyShow
  package2nix pname (PackageData { versions }) =
    Seq.singleton $ quoted (fromPretty pname) $= seqToSet
      (Map.foldMapWithKey (version2nix pname) versions)
  version2nix pname vnum (VersionData { cabalFileRevisions, metaFile }) =
    Seq.singleton $ quoted (fromPretty vnum) $= mkRecSet
      ( ("revision" $= mkSym (revName $ length cabalFileRevisions - 1))
      : ("sha256" $= mkStr (fromString $ parseMetaData pname vnum metaFile Map.! "sha256"))
      : zipWith (revBinding (packagePath pname) vnum) cabalFileRevisions [(0 :: Int) ..]
      )
  revName revNum = "r" <> fromString (show revNum)
  revBinding ppath vnum cabalFile revNum =
    let name :: (IsString a, Semigroup a) => a
        name    = revName revNum
        revPath = "." </> ppath </> fromPretty vnum </> name
    in  name $= mkNonRecSet
          [ "outPath" $= mkRelPath (revPath <.> "nix")
          , "cabalFile" $= mkRelPath (revPath <.> "cabal")
          , "cabalSha256" $= mkStr (decodeUtf8 $ Base16.encode $ hashlazy cabalFile)
          ]
  packagePath pname =
    BS.unpack (BS.take 30 $ Base16.encode $ hash $ fromPretty pname) ++ "-" ++ fromPretty pname
