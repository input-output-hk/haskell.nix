{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where

import Control.Monad.IO.Class (liftIO)
import Language.Haskell.TH.Syntax (Exp(..), Lit(..))
import Data.Text as T

#ifdef MIN_VERSION_HsOpenSSL
import OpenSSL (withOpenSSL)
import OpenSSL.BN (withBN)
#endif

#ifdef MIN_VERSION_libsodium
import Libsodium (sodium_init)
#endif

#ifdef MIN_VERSION_double_conversion
import Data.Double.Conversion.Text (toShortest)
#endif

#ifdef MIN_VERSION_HsOpenSSL
x = $(liftIO (withOpenSSL (withBN 0 (\_ -> return (LitE (IntegerL 0))))))
#endif
#ifdef MIN_VERSION_libsodium
y = $(liftIO (sodium_init >> return (LitE (IntegerL 0))))
#endif
#ifdef MIN_VERSION_double_conversion
z = $(liftIO (return (LitE (IntegerL (fromIntegral (T.length (toShortest 1.0)))))))
#endif
