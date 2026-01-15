{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ProjectPlanOutput (
  -- * Plan output
  writePlanExternalRepresentation,
) where

import qualified Data.ByteString.Builder as BB
import qualified Data.Map as Map

import System.FilePath

import Distribution.System
import Distribution.InstalledPackageInfo (InstalledPackageInfo)
import Distribution.Package
import qualified Distribution.PackageDescription as PD
import Distribution.Types.ComponentName
import Distribution.Types.Version (mkVersion)
import Distribution.Utils.Path (makeSymbolicPath, getSymbolicPath)
import qualified Distribution.Types.ParStrat as ParStrat
import qualified Distribution.Verbosity as Verbosity

import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.BuildPaths (
  buildInfoPref,
  dllExtension,
  exeExtension,
 )
import Distribution.Simple.Compiler (showCompilerId)
import Distribution.Simple.Utils
import Distribution.Simple.Command (CommandUI(..), commandShowOptions)
import qualified Distribution.Simple.Setup as Cabal
import Distribution.Simple.Program.Db (defaultProgramDb)

import Distribution.Client.BuildReports.Types (ReportLevel(..))
import Distribution.Client.DistDirLayout
import Distribution.Client.HashValue (hashValue, showHashValue)
import Distribution.Client.ProjectOrchestration (BuildTimeSettings(..))
import Distribution.Client.ProjectPlanning
import Distribution.Client.ProjectPlanning.Types
import Distribution.Client.Types.ConfiguredId (confInstId)
import Distribution.Client.Types.PackageLocation (PackageLocation (..))
import Distribution.Client.Types.ReadyPackage (GenericReadyPackage(..))
import Distribution.Client.Types.Repo (RemoteRepo (..), Repo (..))
import Distribution.Client.Types.SourceRepo (SourceRepoMaybe, SourceRepositoryPackage (..))
import Distribution.Client.Version (cabalInstallVersion)
import qualified Distribution.Client.InstallPlan as InstallPlan
import qualified Distribution.Client.Utils.Json as J
import qualified Distribution.Solver.Types.ComponentDeps as ComponentDeps

import Distribution.Client.Compat.Prelude
import Prelude ()

-----------------------------------------------------------------------------
-- Writing plan.json files
--

{- | Write out a representation of the elaborated install plan.

This is for the benefit of debugging and external tools like editors.
-}
writePlanExternalRepresentation ::
  DistDirLayout ->
  ElaboratedInstallPlan ->
  ElaboratedSharedConfig ->
  Map (PackageId, ComponentName) [AvailableTarget (UnitId, ComponentName)] ->
  IO ()
writePlanExternalRepresentation
  distDirLayout
  elaboratedInstallPlan
  elaboratedSharedConfig
  targets =
    writeFileAtomic (distProjectCacheFile distDirLayout "plan.json")
      $ BB.toLazyByteString
      . J.encodeToBuilder
      $ encodePlanAsJson distDirLayout elaboratedInstallPlan elaboratedSharedConfig targets

{- | Renders a subset of the elaborated install plan in a semi-stable JSON
format.
-}
encodePlanAsJson ::
  DistDirLayout ->
  ElaboratedInstallPlan ->
  ElaboratedSharedConfig ->
  Map (PackageId, ComponentName) [AvailableTarget (UnitId, ComponentName)] ->
  J.Value
encodePlanAsJson distDirLayout elaboratedInstallPlan elaboratedSharedConfig targets =
  -- TODO: [nice to have] include all of the sharedPackageConfig and all of
  --      the parts of the elaboratedInstallPlan
  J.object
    [ "cabal-version" J..= jdisplay cabalInstallVersion
    , "cabal-lib-version" J..= jdisplay cabalVersion
    , "compiler-id"
        J..= (J.String . showCompilerId . pkgConfigCompiler)
          elaboratedSharedConfig
    , "os" J..= jdisplay os
    , "arch" J..= jdisplay arch
    , "install-plan" J..= installPlanToJ elaboratedInstallPlan
    , "targets" J..= targetsToJ targets
    ]
 where
  plat :: Platform
  plat@(Platform arch os) = pkgConfigPlatform elaboratedSharedConfig

  installPlanToJ :: ElaboratedInstallPlan -> [J.Value]
  installPlanToJ = map planPackageToJ . InstallPlan.toList

  targetsToJ :: Map (PackageId, ComponentName) [AvailableTarget (UnitId, ComponentName)] -> [J.Value]
  targetsToJ = map targetToJ . Map.toList

  targetToJ :: ((PackageId, ComponentName), [AvailableTarget (UnitId, ComponentName)]) -> J.Value
  targetToJ ((pkgId, componentName), ts) =
    J.object
      [ "pkg-name" J..= jdisplay (pkgName pkgId)
      , "pkg-version" J..= jdisplay (pkgVersion pkgId)
      , "component-name" J..= jdisplay componentName
      , "available" J..= map availableTargetToJ ts
      ]

  availableTargetToJ :: AvailableTarget (UnitId, ComponentName) -> J.Value
  availableTargetToJ target =
    case availableTargetStatus target of
      TargetDisabledByUser -> J.String "TargetDisabledByUser"
      TargetDisabledBySolver -> J.String "TargetDisabledBySolver"
      TargetNotBuildable -> J.String "TargetNotBuildable"
      TargetNotLocal -> J.String "TargetNotLocal"
      TargetBuildable (unitId, componentName) requested ->
        J.object
          [ "id" J..= jdisplay unitId
          , "component-name" J..= jdisplay componentName
          , "build-by-default" J..= (requested == TargetRequestedByDefault)
          ]

  planPackageToJ :: ElaboratedPlanPackage -> J.Value
  planPackageToJ pkg =
    case pkg of
      InstallPlan.PreExisting ipi -> installedPackageInfoToJ ipi
      InstallPlan.Configured elab -> elaboratedPackageToJ False elab
      InstallPlan.Installed elab -> elaboratedPackageToJ True elab
  -- Note that the plan.json currently only uses the elaborated plan,
  -- not the improved plan. So we will not get the Installed state for
  -- that case, but the code supports it in case we want to use this
  -- later in some use case where we want the status of the build.

  installedPackageInfoToJ :: InstalledPackageInfo -> J.Value
  installedPackageInfoToJ ipi =
    -- Pre-existing packages lack configuration information such as their flag
    -- settings or non-lib components. We only get pre-existing packages for
    -- the global/core packages however, so this isn't generally a problem.
    -- So these packages are never local to the project.
    --
    J.object
      [ "type" J..= J.String "pre-existing"
      , "id" J..= (jdisplay . installedUnitId) ipi
      , "pkg-name" J..= (jdisplay . pkgName . packageId) ipi
      , "pkg-version" J..= (jdisplay . pkgVersion . packageId) ipi
      , "depends" J..= map jdisplay (installedDepends ipi)
      ]

  elaboratedPackageToJ :: Bool -> ElaboratedConfiguredPackage -> J.Value
  elaboratedPackageToJ isInstalled elab =
    J.object
      $ [ "type"
            J..= J.String
              ( if isInstalled
                  then "installed"
                  else "configured"
              )
        , "id" J..= (jdisplay . installedUnitId) elab
        , "pkg-name" J..= (jdisplay . pkgName . packageId) elab
        , "pkg-version" J..= (jdisplay . pkgVersion . packageId) elab
        , "flags"
            J..= J.object
              [ PD.unFlagName fn J..= v
              | (fn, v) <- PD.unFlagAssignment (elabFlagAssignment elab)
              ]
        , "style" J..= J.String (style2str (elabLocalToProject elab) (elabBuildStyle elab))
        , "pkg-src" J..= packageLocationToJ (elabPkgSourceLocation elab)
        ]
      ++ [ "pkg-cabal-sha256" J..= J.String (showHashValue hash)
         | Just hash <- [fmap hashValue (elabPkgDescriptionOverride elab)]
         ]
      ++ [ "pkg-src-sha256" J..= J.String (showHashValue hash)
         | Just hash <- [elabPkgSourceHash elab]
         ]
      ++ ( case elabBuildStyle elab of
            BuildInplaceOnly _ ->
              ["dist-dir" J..= J.String dist_dir, buildInfoFileLocation]
            BuildAndInstall ->
              -- TODO: install dirs?
              []
         )
      ++ case elabPkgOrComp elab of
        ElabPackage pkg ->
          let components =
                J.object
                  $ [ comp2str c
                        J..= J.object (
                                  [ "depends" J..= map ((jdisplay . confInstId) . fst) ldeps
                                  , "exe-depends" J..= map (jdisplay . confInstId) edeps
                                  ] ++ bin_file c)
                    | (c, (ldeps, edeps)) <-
                        ComponentDeps.toList
                          $ ComponentDeps.zip
                            (pkgLibDependencies pkg)
                            (pkgExeDependencies pkg)
                    ]
           in ["components" J..= components]
        ElabComponent comp ->
          [ "depends" J..= map ((jdisplay . confInstId) . fst) (elabLibDependencies elab)
          , "exe-depends" J..= map jdisplay (elabExeDependencies elab)
          , "component-name" J..= J.String (comp2str (compSolverName comp))
          ]
            ++ bin_file (compSolverName comp)
      ++ [
        "configure-args" J..= j_string_list (renderFlags (Cabal.configureCommand defaultProgramDb) configFlags configArgs),
        "build-args" J..= j_string_list (renderFlags (Cabal.buildCommand defaultProgramDb) buildFlags buildArgs),
        "copy-args" J..= j_string_list (renderFlags Cabal.copyCommand copyFlags copyArgs),
        "haddock-args" J..= j_string_list (renderFlags Cabal.haddockCommand haddockFlags haddockArgs),
        "test-args" J..= j_string_list (renderFlags Cabal.testCommand testFlags testArgs),
        "benchmark-args" J..= j_string_list (renderFlags Cabal.benchmarkCommand benchFlags benchArgs),
        "repl-args" J..= j_string_list (renderFlags (Cabal.replCommand defaultProgramDb) replFlags replArgs)
        ]
   where
    j_string_list = J.Array . map J.String

    -- The setupHs* functions are defined in Distribution.Client.ProjectPlanning
    -- and are intended to be opaque to the plan execution layer. We make a concession
    -- in copyFlags since it's trivial and we need to adapt it to be agnostic to the
    -- build directory

    -- NOTE: We should filter these flags since the Setup.hs we are going
    -- to invoke might not support all features that this version of cabal-install
    -- supports.

    commonFlags :: Cabal.CommonSetupFlags
    commonFlags = mempty

    configArgs = setupHsConfigureArgs elab
    configFlags =
        runIdentity $
        setupHsConfigureFlags
          (fmap makeSymbolicPath . Identity)
          elaboratedInstallPlan
          (ReadyPackage elab)
          elaboratedSharedConfig
          commonFlags

    buildArgs = setupHsBuildArgs elab
    buildFlags =
      setupHsBuildFlags
        Cabal.NoFlag
        elab
        elaboratedSharedConfig
        commonFlags

    -- Simplified version
    copyArgs = setupHsBuildArgs elab
    copyFlags =
        Cabal.CopyFlags
            { copyCommonFlags = commonFlags
            , copyDest = Cabal.NoFlag
            }

    testArgs = setupHsTestArgs elab
    testFlags =
        setupHsTestFlags
          elab
          commonFlags

    benchArgs = setupHsBenchArgs elab
    benchFlags =
      setupHsBenchFlags
        elab
        elaboratedSharedConfig
        commonFlags

    replArgs = setupHsReplArgs elab
    replFlags =
      setupHsReplFlags
        elab
        elaboratedSharedConfig
        commonFlags

    haddockArgs = setupHsHaddockArgs elab
    haddockFlags =
      setupHsHaddockFlags
          elab
          elaboratedSharedConfig
          buildTimeSettings
          commonFlags

    -- NOTE: this is only used in setupHsHaddockFlags (for
    -- buildSettingHaddockOpen),and it is not used during planning. I filled it
    -- with reasonable defaults but it's mostly going to be ignored.
    buildTimeSettings = BuildTimeSettings {
      buildSettingDryRun = False,
      buildSettingOnlyDeps = False,
      buildSettingOnlyDownload = False,
      buildSettingSummaryFile = mempty,
      buildSettingLogFile = Nothing,
      buildSettingLogVerbosity = Verbosity.normal,
      buildSettingBuildReports = NoReports,
      buildSettingSymlinkBinDir = [],
      buildSettingNumJobs = ParStrat.Serial,
      buildSettingKeepGoing = False,
      buildSettingOfflineMode = False,
      buildSettingKeepTempFiles = False,
      buildSettingRemoteRepos = [],
      buildSettingLocalNoIndexRepos = [],
      -- non-empty string for troubleshooting
      buildSettingCacheDir = "<cache-dir>",
      buildSettingHttpTransport = Nothing,
      buildSettingIgnoreExpiry = False,
      buildSettingReportPlanningFailure = False,
      buildSettingProgPathExtra = [],
      buildSettingHaddockOpen = False
    }


    -- \| Only add build-info file location if the Setup.hs CLI
    -- is recent enough to be able to generate build info files.
    -- Otherwise, write 'null'.
    --
    -- Consumers of `plan.json` can use the nullability of this file location
    -- to indicate that the given component uses `build-type: Custom`
    -- with an old lib:Cabal version.
    buildInfoFileLocation :: J.Pair
    buildInfoFileLocation
      | elabSetupScriptCliVersion elab < mkVersion [3, 7, 0, 0] =
          "build-info" J..= J.Null
      | otherwise =
          "build-info" J..= J.String (getSymbolicPath (buildInfoPref (makeSymbolicPath dist_dir)))

    packageLocationToJ :: PackageLocation (Maybe FilePath) -> J.Value
    packageLocationToJ pkgloc =
      case pkgloc of
        LocalUnpackedPackage local ->
          J.object
            [ "type" J..= J.String "local"
            , "path" J..= J.String local
            ]
        LocalTarballPackage local ->
          J.object
            [ "type" J..= J.String "local-tar"
            , "path" J..= J.String local
            ]
        RemoteTarballPackage uri _ ->
          J.object
            [ "type" J..= J.String "remote-tar"
            , "uri" J..= J.String (show uri)
            ]
        RepoTarballPackage repo _ _ ->
          J.object
            [ "type" J..= J.String "repo-tar"
            , "repo" J..= repoToJ repo
            ]
        RemoteSourceRepoPackage srcRepo _ ->
          J.object
            [ "type" J..= J.String "source-repo"
            , "source-repo" J..= sourceRepoToJ srcRepo
            ]

    repoToJ :: Repo -> J.Value
    repoToJ repo =
      case repo of
        RepoLocalNoIndex{..} ->
          J.object
            [ "type" J..= J.String "local-repo-no-index"
            , "path" J..= J.String repoLocalDir
            ]
        RepoRemote{..} ->
          J.object
            [ "type" J..= J.String "remote-repo"
            , "uri" J..= J.String (show (remoteRepoURI repoRemote))
            ]
        RepoSecure{..} ->
          J.object
            [ "type" J..= J.String "secure-repo"
            , "uri" J..= J.String (show (remoteRepoURI repoRemote))
            ]

    sourceRepoToJ :: SourceRepoMaybe -> J.Value
    sourceRepoToJ SourceRepositoryPackage{..} =
      J.object
        $ filter ((/= J.Null) . snd)
          [ "type" J..= jdisplay srpType
          , "location" J..= J.String srpLocation
          , "branch" J..= fmap J.String srpBranch
          , "tag" J..= fmap J.String srpTag
          , "subdir" J..= fmap J.String srpSubdir
          ]

    dist_dir :: FilePath
    dist_dir =
      distBuildDirectory
        distDirLayout
        (elabDistDirParams elaboratedSharedConfig elab)

    bin_file :: ComponentDeps.Component -> [J.Pair]
    bin_file c = case c of
      ComponentDeps.ComponentExe s -> bin_file' s
      ComponentDeps.ComponentTest s -> bin_file' s
      ComponentDeps.ComponentBench s -> bin_file' s
      ComponentDeps.ComponentFLib s -> flib_file' s
      _ -> []
    bin_file' s =
      ["bin-file" J..= J.String bin]
     where
      bin =
        if isInplaceBuildStyle (elabBuildStyle elab)
          then dist_dir </> "build" </> prettyShow s </> prettyShow s <.> exeExtension plat
          else InstallDirs.bindir (elabInstallDirs elab) </> prettyShow s <.> exeExtension plat

    flib_file' :: (Pretty a, Show a) => a -> [J.Pair]
    flib_file' s =
      ["bin-file" J..= J.String bin]
     where
      bin =
        if isInplaceBuildStyle (elabBuildStyle elab)
          then dist_dir </> "build" </> prettyShow s </> ("lib" ++ prettyShow s) <.> dllExtension plat
          else InstallDirs.bindir (elabInstallDirs elab) </> ("lib" ++ prettyShow s) <.> dllExtension plat

comp2str :: ComponentDeps.Component -> String
comp2str = prettyShow

style2str :: Bool -> BuildStyle -> String
style2str True _ = "local"
style2str False (BuildInplaceOnly OnDisk) = "inplace"
style2str False (BuildInplaceOnly InMemory) = "interactive"
style2str False BuildAndInstall = "global"

jdisplay :: (Pretty a) => a -> J.Value
jdisplay = J.String . prettyShow

renderFlags :: CommandUI flags -> flags -> [String] -> [String]
renderFlags cmd flags extraArgs =
    commandName cmd : commandShowOptions cmd flags ++ extraArgs
