{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Control.Monad.IO.Class (liftIO)
import OpenSSL (withOpenSSL)
import OpenSSL.BN (withBN)
import Libsodium (sodium_init)
import Language.Haskell.TH.Syntax (Exp(..), Lit(..))
import Data.Text as T
import Data.Double.Conversion.Text (toShortest)

x = $(liftIO (withOpenSSL (withBN 0 (\_ -> return (LitE (IntegerL 0))))))
y = $(liftIO (sodium_init >> return (LitE (IntegerL 0))))
z = $(liftIO (return (LitE (IntegerL (fromIntegral (T.length (toShortest 1.0)))))))
