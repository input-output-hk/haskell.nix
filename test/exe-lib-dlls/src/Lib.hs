module Lib where

import Control.Monad.IO.Class (liftIO)
import OpenSSL (withOpenSSL)
import OpenSSL.BN (withBN)
import Libsodium (sodium_init)
import Data.Text as T
import Data.Double.Conversion.Text (toShortest)

someFunction = do
  withOpenSSL (withBN 0 (\_ -> return ()))
  sodium_init
  print (T.length (toShortest 1.0))

