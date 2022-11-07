{-# LANGUAGE NamedFieldPuns #-}

import qualified Cabal2Nix
import qualified Data.ByteString.Lazy as BSL
import Data.Foldable (for_)
import Distribution.Client.GlobalFlags
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), defaultNixStyleFlags, nixStyleOptions)
import Distribution.Client.ProjectConfig
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanOutput (writePlanExternalRepresentation)
import Distribution.Client.ProjectPlanning (ElaboratedConfiguredPackage (..), rebuildInstallPlan)
import Distribution.Client.Setup
import Distribution.Package (pkgName)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Command
import Distribution.Simple.Flag
import qualified Distribution.Simple.Utils as Cabal
import Distribution.Verbosity (Verbosity)
import qualified Distribution.Verbosity as Verbosity
import Nix.Pretty (prettyNix)
import Prettyprinter (Doc)
import Prettyprinter.Render.Text (hPutDoc)
import System.Environment (getArgs)
import System.FilePath
import System.IO (IOMode (WriteMode), hClose, openFile)
import Distribution.Client.DistDirLayout (DistDirLayout(distProjectCacheFile))
import System.Directory (renameFile)

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
  ProjectBaseContext {distDirLayout, cabalDirLayout, projectConfig, localPackages} <-
    establishProjectBaseContext verbosity cliConfig OtherCommand

  -- Two variants of the install plan are returned: with and without
  -- packages from the store. That is, the "improved" plan where source
  -- packages are replaced by pre-existing installed packages from the
  -- store (when their ids match), and also the original elaborated plan
  -- which uses primarily source packages.
  (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, _tis, _at) <-
    rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages

  let outputDir = "make-install-plan-out"
  Cabal.createDirectoryIfMissingVerbose verbosity True outputDir

  -- Write plan.json
  let planJsonPath = outputDir </> "plan.json"
  Cabal.notice verbosity $ "Writing plan.json to " ++ planJsonPath
  writePlanExternalRepresentation distDirLayout elaboratedPlan elaboratedSharedConfig
  renameFile (distProjectCacheFile distDirLayout "plan.json") planJsonPath

  -- write cabal.freeze
  let cabalFreezeFile = outputDir </> "cabal.project.freeze"
  Cabal.notice verbosity $ "Wrote freeze file to " ++ cabalFreezeFile
  writeProjectConfigFile cabalFreezeFile projectConfig

  -- write cabal files and their nix version
  let cabalFilesDir = outputDir </> "cabal-files"
  Cabal.createDirectoryIfMissingVerbose verbosity True cabalFilesDir
  Cabal.notice verbosity $ "Writing cabal files to " ++ cabalFilesDir

  let ecps = [ecp | InstallPlan.Configured ecp <- InstallPlan.toList elaboratedPlan, not $ elabLocalToProject ecp]

  for_ ecps $
    \ElaboratedConfiguredPackage
       { elabPkgSourceId,
         elabPkgDescriptionOverride
       } -> do
        let pkgFile = cabalFilesDir </> prettyShow (pkgName elabPkgSourceId) <.> "cabal"
        let nixFile = cabalFilesDir </> prettyShow (pkgName elabPkgSourceId) <.> "nix"
        for_ elabPkgDescriptionOverride $ \pkgTxt ->
          do
            -- raw
            Cabal.info verbosity $ "Writing cabal file for " ++ prettyShow elabPkgSourceId ++ " to " ++ pkgFile
            BSL.writeFile pkgFile pkgTxt
            -- nix
            Cabal2Nix.cabal2nix False Cabal2Nix.MinimalDetails (Just (Cabal2Nix.Path pkgFile)) (Cabal2Nix.OnDisk pkgFile)
            >>= writeDoc nixFile . prettyNix

writeDoc :: FilePath -> Doc ann -> IO ()
writeDoc file doc = do
  handle <- openFile file WriteMode
  hPutDoc handle doc
  hClose handle
