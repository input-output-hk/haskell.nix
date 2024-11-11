module Plugin (plugin) where

import GHC.Plugins
import System.IO
import Control.Monad

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  }

install :: CorePlugin
install files passes = do
  liftIO . forM files $ flip writeFile ""
  pure passes
