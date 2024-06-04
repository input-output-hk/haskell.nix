{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Control.Monad.IO.Class (liftIO)
import Language.Haskell.TH.Syntax (Exp(..), Lit(..))
import MyLib (someFunc)

a = $(liftIO (LitE . IntegerL . fromIntegral <$> someFunc))
