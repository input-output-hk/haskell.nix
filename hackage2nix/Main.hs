{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import           Cabal2Nix
import           Cabal2Nix.Util                           ( quoted )
import           Control.Applicative                      ( liftA2 )
import           Control.Monad.Trans.State.Strict
import           Crypto.Hash.SHA256                       ( hashlazy )
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
import           Nix.Expr
import           Nix.Pretty                               ( prettyNix )
import           System.Directory                         ( createDirectoryIfMissing
                                                          )
import           System.Environment                       ( getArgs )
import           System.FilePath                          ( (</>)
                                                          , (<.>)
                                                          )

main :: IO ()
main = do
  [out] <- getArgs
  db    <- U.readTarball Nothing =<< hackageTarball

  let (defaultNix, cabalFiles) =
        runState (fmap seqToSet $ foldMapWithKeyA package2nix db) mempty

  createDirectoryIfMissing False out
  writeFile (out </> "default.nix") $ show $ prettyNix defaultNix
  createDirectoryIfMissing False (out </> "hackage")

  for_ cabalFiles $ \(cabalFile, pname, path) -> do
    gpd <- cabal2nix False MinimalDetails Nothing $ InMemory Nothing pname $ BL.toStrict cabalFile
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

seqToSet :: Seq (Binding NExpr) -> NExpr
seqToSet = mkNonRecSet . toList

fromPretty :: (Pretty a, IsString b) => a -> b
fromPretty = fromString . prettyShow

package2nix :: PackageName -> U.PackageData -> GPDWriter (Seq (Binding NExpr))
package2nix pname (U.PackageData { U.versions }) = do
  versionBindings <- foldMapWithKeyA (version2nix pname) versions
  return $ Seq.singleton $ quoted (fromPretty pname) $= seqToSet versionBindings

version2nix
  :: PackageName -> Version -> U.VersionData -> GPDWriter (Seq (Binding NExpr))
version2nix pname vnum (U.VersionData { U.cabalFileRevisions, U.metaFile }) =
  do
    revisionBindings <- sequenceA
      $ zipWith (revBinding pname vnum) cabalFileRevisions [0 ..]
    return $ Seq.singleton $ quoted (fromPretty vnum) $= mkRecSet
      [ "sha256" $= mkStr
        (fromString $ P.parseMetaData pname vnum metaFile Map.! "sha256")
      , "revisions" $= mkNonRecSet
        (  fmap (uncurry ($=)) revisionBindings
        ++ ["default" $= (mkSym "revisions" @. fst (last revisionBindings))]
        )
      ]

revBinding
  :: PackageName
  -> Version
  -> BL.ByteString
  -> Integer
  -> GPDWriter (Text, NExpr)
revBinding pname vnum cabalFile revNum = do
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
  return $ (,) revName $ mkNonRecSet
    [ "outPath" $= mkRelPath revPath
    , "revNum" $= mkInt revNum
    , "sha256" $= mkStr (decodeUtf8 cabalHash)
    ]
