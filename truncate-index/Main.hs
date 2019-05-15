-- I am a sinner!
{-# language RecordWildCards #-}

module Main (main) where

import Data.Maybe
import Data.Time.Clock
import Data.Time.Format
import Data.Time.Clock.POSIX
import Codec.Archive.Tar as Tar
import Codec.Archive.Tar.Entry as Tar
import Data.ByteString.Lazy as BS hiding (filter)
import Codec.Compression.GZip as GZip

import Options.Applicative hiding (option)
import Data.Semigroup ((<>))

-- | Parse a hackage index state like "2019-01-01T12:00:00Z"
parseIndexState :: String -> Maybe UTCTime
parseIndexState = parseTimeM True defaultTimeLocale (iso8601DateFormat (Just "%T%Z"))

-- | Convert the standard 'UTCTime' type into the 'EpochTime' used by the @tar@
-- library.
toEpochTime :: UTCTime -> EpochTime
toEpochTime = floor . utcTimeToPOSIXSeconds

-- | Filter Haskell Index
filterHaskellIndex :: FilePath -> String -> FilePath -> IO ()
filterHaskellIndex orig indexState out =
  BS.writeFile out . GZip.compress . Tar.write . f . Tar.read . GZip.decompress =<< BS.readFile orig
  where f = filter ((<= ts) . Tar.entryTime) . toList
        ts = toEpochTime . fromJust . parseIndexState $ indexState

-- | Convert @Entries e@ to a list while throwing an error on failure.
toList :: Show e => Entries e -> [Entry]
toList (Next e es) = e:(toList es)
toList Done = []
toList (Fail e) = error (show e)

--------------------------------------------------------------------------------
-- CLI Argument
data Args = Args
  { argOutput :: FilePath
  , argInput  :: FilePath
  , argIndexState :: String
  } deriving Show

args :: Parser Args
args = Args
  <$> strOption ( long "output" <> short 'o' <> value "00-index.tar.gz" <> showDefault <> metavar "FILE" <> help "The output index" )
  <*> strOption ( long "input" <> short 'i' <> value "01-index.tar.gz" <> showDefault <> metavar "FILE" <> help "The input index" )
  <*> strOption ( long "indexState" <> short 's' <> metavar "INDEX" <> help "Index State ( YYYY-MM-DDTHH:MM:SSZ )" )

main :: IO ()
main = execParser opts >>= \Args{..} -> filterHaskellIndex argInput argIndexState argOutput
  where opts = info (args <**> helper)
          ( fullDesc
         <> progDesc "Generate a truncated Hackage index"
         <> header "truncate-index - a hackage index truncater" )
