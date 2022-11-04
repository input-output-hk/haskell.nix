{-# LANGUAGE NamedFieldPuns #-}

import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Distribution.Client.Config (getCabalDir)
import Distribution.Client.DistDirLayout
  ( CabalDirLayout,
    DistDirLayout (..),
    defaultDistDirLayout,
    mkCabalDirLayout,
  )
import Distribution.Client.GlobalFlags
import Distribution.Client.HttpUtils (configureTransport)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags, nixStyleOptions)
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
import Distribution.Client.Setup
import Distribution.Compat.Directory (makeAbsolute)
import Distribution.Package (pkgName)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Command
import Distribution.Simple.Flag
import qualified Distribution.Simple.Utils as Cabal
import Distribution.Verbosity (Verbosity, moreVerbose)
import qualified Distribution.Verbosity as Verbosity
import System.Environment (getArgs)
import System.FilePath
import Distribution.Client.ProjectOrchestration

main :: IO ()
main = do
  args <- getArgs
  case commandParseArgs cmdUI True args of
    CommandHelp help -> putStrLn (help "something")
    CommandList opts -> putStrLn $ "commandList" ++ show opts
    CommandErrors errs -> putStrLn $ "commandErrors: " ++ show errs
    CommandReadyToGo (mkflags, _commandParse) ->
      let globalFlags = defaultGlobalFlags
          flags@NixStyleFlags {configFlags} = mkflags (commandDefaultFlags cmdUI)
          verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity configFlags)
          cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty
       in installPlanAction verbosity cliConfig

cmdUI :: CommandUI (NixStyleFlags ())
cmdUI =
  CommandUI
    { commandName = "",
      commandSynopsis = "Makes an install-plan",
      commandUsage = ("Usage: " ++),
      commandDescription = Nothing,
      commandNotes = Nothing,
      commandDefaultFlags = defaultNixStyleFlags (),
      commandOptions = nixStyleOptions (const [])
    }

installPlanAction :: Verbosity -> ProjectConfig -> IO ()
installPlanAction verbosity cliConfig = do

  ProjectBaseContext {distDirLayout, cabalDirLayout, projectConfig, localPackages}
    <- establishProjectBaseContext verbosity cliConfig OtherCommand

  -- Two variants of the install plan are returned: with and without
  -- packages from the store. That is, the "improved" plan where source
  -- packages are replaced by pre-existing installed packages from the
  -- store (when their ids match), and also the original elaborated plan
  -- which uses primarily source packages.
  (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, _tis, _at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  Cabal.notice verbosity $ "Writing plan.json to " ++ distProjectCacheFile distDirLayout "plan.json"
  writePlanExternalRepresentation distDirLayout elaboratedPlan elaboratedSharedConfig

  let cabalFreezeFile = distProjectFile distDirLayout "freeze"
  Cabal.notice verbosity $ "Wrote freeze file to " ++ cabalFreezeFile
  writeProjectConfigFile cabalFreezeFile projectConfig

  let cabalFilesDir = distDirectory distDirLayout </> "cabal-files"
  Cabal.createDirectoryIfMissingVerbose verbosity True cabalFilesDir
  Cabal.notice verbosity $ "Writing cabal files to " ++ cabalFilesDir

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
