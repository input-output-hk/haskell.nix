module Stack2nix.CLI
  ( Args(..)
  , parseStack2nixArgs
  ) where

import Options.Applicative hiding (option)
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


parseStack2nixArgs :: IO Args
parseStack2nixArgs = execParser opts
  where opts = info (args <**> helper)
          ( fullDesc
         <> progDesc "Generate a nix expression from a stack.yaml file"
         <> header "stack-to-nix - a stack to nix converter" )
