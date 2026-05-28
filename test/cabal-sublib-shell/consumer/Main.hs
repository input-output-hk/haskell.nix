module Main where

import Provider (mainMessage)
import Provider.Slib (slibMessage)

main :: IO ()
main = do
  putStrLn mainMessage
  putStrLn ("direct: " <> slibMessage)
