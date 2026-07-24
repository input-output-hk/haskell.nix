-- | The git-fetch cache shared by @plan-to-nix@ and @stack-to-nix@.
--
-- This used to be two copies (@Plan2Nix.Cache@ and @Stack2nix.Cache@) that
-- drifted apart: the Plan2Nix copy stored a @NOHASH@ placeholder for empty
-- hashes so that the whitespace-delimited format still round-trips through
-- 'words', while the Stack2nix copy wrote a raw empty string (corrupting the
-- cache-file format for local packages), and only the Stack2nix copy forced
-- the reads/writes strictly.  This module keeps the union of the correct
-- behaviours: the @NOHASH@ placeholder *and* the strict IO.
module NixTools.Cache
  ( readCache
  , appendCache
  , cacheHits
  ) where

import Control.DeepSeq ((<$!!>))
import Control.Exception (catch, SomeException(..))

readCache :: FilePath
          -> IO [( String -- url
                 , String -- rev
                 , String -- subdir
                 , String -- sha256
                 , String -- pkgname
                 , String -- nixexpr-path
                 )]
readCache f = fmap (toTuple . words) . lines <$!!> readFile f
  where toTuple [ url, rev, subdir, sha256, pkgname, exprPath ]
          = ( url, rev, subdir, if sha256 == "NOHASH" then "" else sha256, pkgname, exprPath )

-- When we do not need a hash (when the files are local) we store "NOHASH" instead of ""
-- in the file so that the use of `words` function in `readCache` still works.
appendCache :: FilePath -> String -> String -> String -> String -> String -> String -> IO ()
appendCache f url rev subdir sha256 pkgname exprPath = do
  appendFile f $! unwords [ url, rev, subdir, if null sha256 then "NOHASH" else sha256, pkgname, exprPath ] ++ "\n"

cacheHits :: FilePath -> String -> String -> String -> IO [ (String, String) ]
cacheHits f url rev subdir
  = do cache <- catch' (readCache f) (const (pure []))
       return [ ( pkgname, exprPath )
              | ( url', rev', subdir', sha256, pkgname, exprPath ) <- cache
              , url == url'
              , rev == rev'
              , subdir == subdir' ]
  where catch' :: IO a -> (SomeException -> IO a) -> IO a
        catch' = catch
