module Stack2nix.CLI
  ( Args(..)
  , parseStack2nixArgs
  ) where

import Options.Applicative hiding (option)
import Data.Semigroup ((<>))


--------------------------------------------------------------------------------
-- CLI Arguments
data Args = Args
  { argOutputDir :: FilePath
  , argStackYaml :: FilePath
  , argCacheFile :: FilePath
  } deriving Show

-- Argument Parser
args :: Parser Args
args = Args
  <$> strOption ( long "output" <> short 'o' <> metavar "DIR" <> help "Generate output in DIR" )
  <*> strOption ( long "stack-yaml" <> value "stack.yaml" <> showDefault <> metavar "FILE" <> help "Override project stack.yaml" )
  <*> strOption ( long "cache" <> value ".stack-to-nix.cache" <> showDefault <> metavar "FILE" <> help "Dependency cache file" )

parseStack2nixArgs :: IO Args
parseStack2nixArgs = execParser opts
  where opts = info (args <**> helper)
          ( fullDesc
         <> progDesc "Generate a Nix expression for a Haskell package using Stack"
         <> header "stack-to-nix - a stack to nix converter" )
