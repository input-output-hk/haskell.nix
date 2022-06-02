{-# LANGUAGE CPP #-}
import Distribution.Simple

-- https://github.com/snoyberg/conduit#readme

import Conduit
import System.Directory (removeFile)

-- Make sure Cabal and ghc were included
import Distribution.Package ()
#if MIN_VERSION_ghc(9,0,0)
import GHC.SysTools.Ar ()
#else
import Ar ()
#endif

example = do
    -- Pure operations: summing numbers.
    print $ runConduitPure $ yieldMany [1..10] .| sumC

    -- Exception safe file access: copy a file.
    writeFile "input.txt" "This is a test." -- create the source file
    runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt" -- actual copying
    readFile "output.txt" >>= putStrLn -- prove that it worked

    -- Perform transformations.
    print $ runConduitPure $ yieldMany [1..10] .| mapC (+ 1) .| sinkList

    removeFile "input.txt"
    removeFile "output.txt"

main = defaultMain
