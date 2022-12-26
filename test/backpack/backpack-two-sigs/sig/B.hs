module B where

import qualified A
import qualified C

hello :: A.SomeData -> IO ()
hello d = putStrLn $ show d

hello2 :: C.SomeData -> IO ()
hello2 d = putStrLn $ show d