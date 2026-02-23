module Main where

import Lib
import GHC.Driver.Session (DynFlags)

main :: IO ()
main = do
  putStrLn greeting
  putStrLn "Successfully linked with GHC library!"
  putStrLn $ "DynFlags type exists: " ++ show (const True (undefined :: DynFlags))
