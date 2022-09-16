{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad (join)
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Distribution.Client.Config (getCabalDir)
import Distribution.Client.DistDirLayout
  ( CabalDirLayout,
    DistDirLayout (distProjectCacheFile),
    defaultDistDirLayout,
    mkCabalDirLayout,
  )
import Distribution.Client.HttpUtils (configureTransport)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectPlanOutput (writePlanExternalRepresentation)
import Distribution.Client.ProjectPlanning
  ( ElaboratedConfiguredPackage
      ( ElaboratedConfiguredPackage,
        elabLocalToProject,
        elabPkgDescriptionOverride,
        elabPkgSourceId
      ),
    rebuildInstallPlan,
    rebuildProjectConfig,
  )
import Distribution.Compat.Directory (makeAbsolute)
import Distribution.Package (pkgName)
import Distribution.Parsec (eitherParsec)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Flag
import qualified Distribution.Simple.Utils as Cabal
import Distribution.Verbosity (Verbosity, moreVerbose)
import qualified Distribution.Verbosity as Verbosity
import Options.Applicative
import System.FilePath

main :: IO ()
main =
  join $
    execParser $
      info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Extracts a cabal install plan")
  where
    optionsParser = do
      verbosity <-
        option
          (eitherReader eitherParsec)
          ( long "verbosity"
              <> metavar "VERBOSITY"
              <> value Verbosity.normal
              <> help "Verbosity"
          )
      inputDir <- optional (argument str (metavar "INPUT-DIR"))
      outputDir <- argument str (metavar "OUTPUT-DIR" <> value "./out")
      pure $ doMain verbosity inputDir outputDir

doMain :: Verbosity -> Maybe FilePath -> [Char] -> IO ()
doMain verbosity inputDir outputDir = do
  Right projectRoot <- findProjectRoot inputDir Nothing
  let distDirLayout = defaultDistDirLayout projectRoot (Just outputDir)

  httpTransport <- configureTransport verbosity mempty mempty

  (projectConfig, localPackages) <-
    rebuildProjectConfig
      -- more verbose here to list the project files which have affected
      -- the project configuration with no extra options
      (moreVerbose verbosity)
      httpTransport
      distDirLayout
      mempty

  cabalDirLayout <- cabalDirLayoutFromProjectConfig projectConfig

  -- Two variants of the install plan are returned: with and without
  -- packages from the store. That is, the "improved" plan where source
  -- packages are replaced by pre-existing installed packages from the
  -- store (when their ids match), and also the original elaborated plan
  -- which uses primarily source packages.
  (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, _tis, _at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  putStrLn $ "Writing detailed plan to " ++ outputDir

  Cabal.notice verbosity $ "Writing plan.json to" ++ distProjectCacheFile distDirLayout "plan.json"
  writePlanExternalRepresentation distDirLayout elaboratedPlan elaboratedSharedConfig

  let cabalFreezeFile = outputDir </> "cabal.project.freeze"
  Cabal.notice verbosity $ "Wrote freeze file: " ++ cabalFreezeFile
  writeProjectConfigFile cabalFreezeFile projectConfig

  let cabalFilesDir = outputDir </> "cabal-files"
  Cabal.createDirectoryIfMissingVerbose verbosity True cabalFilesDir
  Cabal.notice verbosity $ "Writing cabal files to" ++ cabalFilesDir

  let ecps = [ecp | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedPlan, not $ elabLocalToProject ecp]

  for_ ecps $
    \ElaboratedConfiguredPackage
       { elabPkgSourceId,
         elabPkgDescriptionOverride
       } -> do
        let pkgFile = cabalFilesDir </> prettyShow (pkgName elabPkgSourceId) <.> "cabal"
        for_ elabPkgDescriptionOverride $ \pkgTxt -> do
          Cabal.info verbosity $ "Writing cabal file for " ++ prettyShow elabPkgSourceId ++ " to " ++ pkgFile
          BSL.writeFile pkgFile pkgTxt

cabalDirLayoutFromProjectConfig :: ProjectConfig -> IO CabalDirLayout
cabalDirLayoutFromProjectConfig
  ProjectConfig
    { projectConfigBuildOnly = ProjectConfigBuildOnly {projectConfigLogsDir},
      projectConfigShared = ProjectConfigShared {projectConfigStoreDir}
    } = do
    cabalDir <- getCabalDir

    let mlogsDir = flagToMaybe projectConfigLogsDir
    mstoreDir <- sequenceA $ makeAbsolute <$> flagToMaybe projectConfigStoreDir
    return $ mkCabalDirLayout cabalDir mstoreDir mlogsDir
