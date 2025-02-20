{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

import System.Environment (getArgs)

import Data.ByteString.Lazy qualified as BSL

import Distribution.Compat.Graph (IsNode (..))
import Distribution.InstalledPackageInfo (InstalledPackageInfo (..))
import Distribution.PackageDescription (
    PkgconfigName,
    PkgconfigVersion,
    buildType,
 )
import Distribution.Pretty (prettyShow)
import Distribution.Simple (
    packageName,
    packageVersion,
    showCompilerId,
 )
import Distribution.Simple.Command (
    CommandParse (..),
    CommandUI (..),
    commandParseArgs,
    commandShowOptions,
 )
import Distribution.Simple.Flag (fromFlagOrDefault)
import Distribution.Simple.Program.Db (defaultProgramDb)
import Distribution.Simple.Setup (configureCommand)
import Distribution.Simple.Utils (fromUTF8LBS)
import Distribution.System (Platform (..))
import Distribution.Types.Flag (
    FlagAssignment,
    unFlagAssignment,
    unFlagName,
 )
import Distribution.Types.LocalBuildConfig (
    BuildOptions (..),
    buildOptionsConfigFlags,
 )
import Distribution.Utils.Json qualified as J
import Distribution.Verbosity qualified as V

import Distribution.Client.GlobalFlags (defaultGlobalFlags)
import Distribution.Client.HashValue (
    HashValue,
    hashValue,
    showHashValue,
 )
import Distribution.Client.InstallPlan qualified as InstallPlan
import Distribution.Client.NixStyleOptions (
    NixStyleFlags (..),
    defaultNixStyleFlags,
    nixStyleOptions,
 )
import Distribution.Client.ProjectConfig (
    ProjectConfig,
    commandLineFlagsToProjectConfig,
 )
import Distribution.Client.ProjectOrchestration (
    CurrentCommand (..),
    ProjectBaseContext (..),
    establishProjectBaseContext,
 )
import Distribution.Client.ProjectPlanning (
    BuildStyle (..),
    ElaboratedConfiguredPackage (..),
    ElaboratedInstallPlan,
    ElaboratedPlanPackage,
    ElaboratedSharedConfig (..),
    rebuildInstallPlan,
 )
import Distribution.Client.ProjectPlanning.Types (
    ElaboratedComponent (..),
    ElaboratedPackage (..),
    ElaboratedPackageOrComponent (..),
    MemoryOrDisk (..),
 )
import Distribution.Client.Setup (ConfigFlags (..))
import Distribution.Client.Types.ConfiguredId (ConfiguredId (..))
import Distribution.Client.Types.PackageLocation (PackageLocation (..))
import Distribution.Client.Types.Repo (
    LocalRepo (..),
    RemoteRepo (..),
    Repo (..),
 )
import Distribution.Client.Types.SourceRepo (SourceRepositoryPackage (..))

import Distribution.Solver.Types.ComponentDeps qualified as CD

import Json

main :: IO ()
main = do
    args <- getArgs
    case commandParseArgs cmdUI True args of
        CommandHelp help -> putStrLn (help "make-install-plan")
        CommandList opts -> putStrLn ("commandList" ++ show opts)
        CommandErrors errs -> putStrLn ("commandErrors: " ++ show errs)
        CommandReadyToGo (mkflags, _commandParse) ->
            let globalFlags = defaultGlobalFlags
                flags@NixStyleFlags{configFlags} = mkflags (commandDefaultFlags cmdUI)
                verbosity = fromFlagOrDefault V.silent (configVerbosity configFlags)
                cliConfig = commandLineFlagsToProjectConfig globalFlags flags mempty
             in installPlanAction verbosity cliConfig

cmdUI :: CommandUI (NixStyleFlags ())
cmdUI =
    CommandUI
        { commandName = ""
        , commandSynopsis = "Makes an install-plan"
        , commandUsage = ("Usage: " ++)
        , commandDescription = Nothing
        , commandNotes = Nothing
        , commandDefaultFlags = defaultNixStyleFlags ()
        , commandOptions = nixStyleOptions (const [])
        }

installPlanAction :: V.Verbosity -> ProjectConfig -> IO ()
installPlanAction verbosity cliConfig = do
    ProjectBaseContext{distDirLayout, cabalDirLayout, projectConfig, localPackages} <-
        establishProjectBaseContext verbosity cliConfig OtherCommand

    (_improvedPlan, elaboratedPlan, elaboratedSharedConfig, _totalIndexState, _activeRepos) <-
        rebuildInstallPlan verbosity distDirLayout cabalDirLayout projectConfig localPackages Nothing

    let ElaboratedSharedConfig{pkgConfigPlatform, pkgConfigCompiler} = elaboratedSharedConfig
        Platform arch os = pkgConfigPlatform

    BSL.putStr . J.renderJson $
        objectJ
            [ "platform" .= prettyShowJ pkgConfigPlatform
            , "os" .= prettyShowJ os
            , "arch" .= prettyShowJ arch
            , "compiler" .= stringJ (showCompilerId pkgConfigCompiler)
            , "install-plan" .= installPlanJ elaboratedPlan
            ]

installPlanJ :: ElaboratedInstallPlan -> Json
installPlanJ elabPlan =
    dictJ
        prettyShow
        (objectJ . planPackageJ)
        (\f -> foldr (\n -> f (nodeKey n) n))
        (InstallPlan.toGraph elabPlan)

planPackageJ :: ElaboratedPlanPackage -> Series
planPackageJ (InstallPlan.PreExisting pkg) =
    "type" .= stringJ "pre-existing" <> installedPackageInfoJ pkg
planPackageJ (InstallPlan.Configured pkg) =
    "type" .= stringJ "configured" <> configuredPackageJ pkg
planPackageJ (InstallPlan.Installed pkg) =
    "type" .= stringJ "installed" <> configuredPackageJ pkg

installedPackageInfoJ :: InstalledPackageInfo -> Series
installedPackageInfoJ InstalledPackageInfo{..} =
    "name" .= prettyShowJ (packageName sourcePackageId)
        <> "version" .= prettyShowJ (packageVersion sourcePackageId)
        <> "depends" .= listJ prettyShowJ depends

configuredPackageJ :: ElaboratedConfiguredPackage -> Series
configuredPackageJ ElaboratedConfiguredPackage{..} =
    "pkg-name" .= prettyShowJ (packageName elabPkgSourceId)
        <> "pkg-version" .= prettyShowJ (packageVersion elabPkgSourceId)
        <> "pkg-source" .= pkgSrcJ elabPkgSourceLocation elabPkgSourceHash
        <> "pkg-description" ?= fmap packageDescriptionOverrideJ elabPkgDescriptionOverride
        <> "build-type" .= prettyShowJ (buildType elabPkgDescription)
        <> "style" .= styleJ elabLocalToProject elabBuildStyle
        <> "flags" .= flagAssignmentJ elabFlagAssignment
        <> "build-options" .= buildOptionsJ elabBuildOptions
        <> "configure-flags" .= listJ stringJ configFlags
        <> "configure-args" .= listJ stringJ elabConfigureScriptArgs
        <> componentsJ elabPkgOrComp
  where
    configFlags =
        commandShowOptions (configureCommand defaultProgramDb) (buildOptionsConfigFlags elabBuildOptions)

styleJ :: Bool -> BuildStyle -> J.Json
styleJ True _ = stringJ "local"
styleJ False (BuildInplaceOnly OnDisk) = stringJ "inplace"
styleJ False (BuildInplaceOnly InMemory) = stringJ "interactive"
styleJ False BuildAndInstall = stringJ "global"

componentsJ :: ElaboratedPackageOrComponent -> Series
componentsJ (ElabPackage ElaboratedPackage{..}) =
    let components =
            objectJ $
                [ prettyShow comp
                    .= objectJ
                        [ "exe-depends" ?= nonEmptyJ (prettyShowJ . confInstId) edeps
                        , "lib-depends" ?= nonEmptyJ (prettyShowJ . confInstId . fst) ldeps
                        ]
                | let deps = CD.zip pkgExeDependencies pkgLibDependencies
                , (comp, (edeps, ldeps)) <- CD.toList deps
                ]
     in "components" .= components
            -- these are not per component
            <> "pkgconfig-depends" ?= nonEmptyJ (uncurry pkgConfigDependJ) pkgPkgConfigDependencies
componentsJ (ElabComponent ElaboratedComponent{..}) =
    "components"
        .= objectJ
            [ prettyShow compSolverName
                .= objectJ
                    [ "exe-depends" ?= nonEmptyJ (prettyShowJ . confInstId) compExeDependencies
                    , "lib-depends" ?= nonEmptyJ (prettyShowJ . confInstId . fst) compLibDependencies
                    , "pkgconfig-depends" ?= nonEmptyJ (uncurry pkgConfigDependJ) compPkgConfigDependencies
                    ]
            ]

packageDescriptionOverrideJ :: BSL.ByteString -> J.Json
packageDescriptionOverrideJ desc =
    objectJ
        [ "text" .= stringJ (fromUTF8LBS desc)
        , "hash" .= stringJ (showHashValue $ hashValue desc)
        ]

pkgSrcJ :: PackageLocation loc -> Maybe HashValue -> J.Json
pkgSrcJ pkgSrcLoc mPkgSrcHash =
    objectJ [packageLocationJ pkgSrcLoc, hash]
  where
    hash = "hash" ?= fmap (stringJ . showHashValue) mPkgSrcHash

packageLocationJ :: PackageLocation local -> Series
packageLocationJ (LocalUnpackedPackage path) =
    "local" .= stringJ path
packageLocationJ (LocalTarballPackage path) =
    "local-tar" .= stringJ path
packageLocationJ (RemoteTarballPackage uri _local) =
    "remote-tar" .= objectJ ["uri" .= showJ uri]
packageLocationJ (RepoTarballPackage repo _packageId _local) =
    case repo of
        (RepoLocalNoIndex lr _repoLocalDir) ->
            "local-repo-no-index" .= objectJ ["path" .= showJ (localRepoPath lr)]
        (RepoRemote rr _repoLocalDir) ->
            "remote-repo" .= objectJ ["uri" .= showJ (remoteRepoURI rr)]
        (RepoSecure rr _repoLocalDir) ->
            "secure-repo" .= objectJ ["uri" .= showJ (remoteRepoURI rr)]
packageLocationJ (RemoteSourceRepoPackage srp _local) =
    "source-repo"
        .= objectJ
            [ "type" .= prettyShowJ (srpType srp)
            , "location" .= stringJ (srpLocation srp)
            , "tag" ?= fmap stringJ (srpTag srp)
            , "branch" ?= fmap stringJ (srpBranch srp)
            , "subdir" ?= fmap stringJ (srpSubdir srp)
            , "commands" ?= nonEmptyJ stringJ (srpCommand srp)
            ]

flagAssignmentJ :: FlagAssignment -> J.Json
flagAssignmentJ = dictJ unFlagName boolJ (foldr . uncurry) . unFlagAssignment

buildOptionsJ :: BuildOptions -> J.Json
buildOptionsJ bo =
    objectJ
        [ "with-vanilla-lib" .= showJ (withVanillaLib bo)
        , "with-prof-lib" .= showJ (withProfLib bo)
        , --  FIXME: this needs a more recent cabal
          -- "with-prof-lib-shared" .= showJ (withProfLibShared bo)
          "with-shared-lib" .= showJ (withSharedLib bo)
        , "with-static-lib" .= showJ (withStaticLib bo)
        , "with-dyn-exe" .= showJ (withDynExe bo)
        , "with-fully-static-exe" .= showJ (withFullyStaticExe bo)
        , "with-prof-exe" .= showJ (withProfExe bo)
        , "with-prof-lib-detail" .= showJ (withProfLibDetail bo)
        , "with-prof-exe-detail" .= showJ (withProfExeDetail bo)
        , "with-optimization" .= showJ (withOptimization bo)
        , "with-debug-info" .= showJ (withDebugInfo bo)
        , "with-ghci-lib" .= showJ (withGHCiLib bo)
        , "split-sections" .= showJ (splitSections bo)
        , "split-objs" .= showJ (splitObjs bo)
        , "strip-exes" .= showJ (stripExes bo)
        , "strip-libs" .= showJ (stripLibs bo)
        , "exe-coverage" .= showJ (exeCoverage bo)
        , "lib-coverage" .= showJ (libCoverage bo)
        , "relocatable" .= showJ (relocatable bo)
        ]

pkgConfigDependJ :: PkgconfigName -> Maybe PkgconfigVersion -> J.Json
pkgConfigDependJ n v =
    objectJ
        [ "name" .= prettyShowJ n
        , "version" ?= fmap prettyShowJ v
        ]
