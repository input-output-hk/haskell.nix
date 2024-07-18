module Main where

import qualified GI.Gtk as Gtk (initCheck)

main :: IO ()
main = do
  Gtk.initCheck
  return ()

