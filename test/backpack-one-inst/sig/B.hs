module B where

import qualified A

hello :: A.SomeData -> IO ()
hello d = putStrLn $ show d