module Lib (message) where

-- A trivial exposed module so the library has content (and a package.db
-- entry whose data-dir the v2 check must point at).
message :: String
message = "hello"
