{-# LANGUAGE OverloadedStrings #-}

-- | Pure regression test for the friendly hackage.nix lookup guard (issue #855).
--
-- 'hackageVersion' is what @stack-to-nix@ / @lts-to-nix@ emit for every package
-- resolved from a snapshot or @extra-deps@. Before #855 it produced a bare
-- @hackage.<pkg>."<ver>"@ selection, so a package or version missing from
-- hackage.nix failed with Nix's opaque @attribute '<ver>' missing@. This test
-- renders the emitted expression and checks the lookups are now wrapped in
-- actionable @builtins.throw@s that name the package/version and point at
-- hackage.nix.
module Main (main) where

import           Cabal2Nix.Util               (hackageVersion, quoted)
import           Control.Monad                (unless)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Nix.Pretty                   (prettyNix)
import           Prettyprinter                (defaultLayoutOptions, layoutPretty)
import           Prettyprinter.Render.Text    (renderStrict)
import           System.Exit                  (exitFailure)
import           System.IO                    (hPutStrLn, stderr)

rendered :: Text
rendered =
  renderStrict
    (layoutPretty defaultLayoutOptions
      (prettyNix (hackageVersion "aeson" (quoted "1.0.0"))))

main :: IO ()
main = do
  let needles = [ "hackage"                 -- still looks the package up in hackage.nix
                , "aeson"                    -- names the package (both lookup and message)
                , "1.0.0"                    -- names the version
                , "builtins.throw"           -- guarded, not a bare selection
                , "hackage.nix package set"  -- actionable wording
                ]
      missing = filter (\n -> not (n `T.isInfixOf` rendered)) needles
  unless (null missing) $ do
    hPutStrLn stderr $
      "hackageVersion output is missing " ++ show missing ++ "\nrendered:\n" ++ T.unpack rendered
    exitFailure
  putStrLn "hackageVersion emits guarded lookups with friendly errors (OK)"
