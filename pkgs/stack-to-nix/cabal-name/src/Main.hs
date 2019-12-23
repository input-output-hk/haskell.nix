module Main where

import Distribution.PackageDescription.Parsec
import Distribution.Types.GenericPackageDescription
import Distribution.Types.PackageDescription
import Distribution.Types.PackageId
import Distribution.Types.PackageName
import Distribution.Verbosity
import System.Environment

main = do
  args <- getArgs
  desc <- readGenericPackageDescription normal (head args)
  print $ (unPackageName . pkgName . package . packageDescription) desc
