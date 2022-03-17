{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Control.Monad.IO.Class (liftIO)
import OpenSSL (withOpenSSL)
import OpenSSL.BN (withBN)
import Libsodium (sodium_init)
import Language.Haskell.TH.Syntax (Exp(..), Lit(..))

x = $(liftIO (withOpenSSL (withBN 0 (\_ -> return (LitE (IntegerL 0))))))
y = $(liftIO (sodium_init >> return (LitE (IntegerL 0))))
