{-# language LambdaCase #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Types.PackageDescription
import Distribution.Types.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Types.InstalledPackageInfo hiding (includeDirs)
import qualified Distribution.Types.InstalledPackageInfo as IPI
import Distribution.Types.PackageName
import System.FilePath
import Control.Monad (filterM, forM_, forM, unless)
import System.Directory (doesFileExist)
import Distribution.Types.Library (libBuildInfo, Library(..))
import Distribution.Types.BuildInfo (cSources, jsSources, includeDirs, emptyBuildInfo, options, extraBundledLibs)
import Distribution.Simple.BuildTarget (readBuildTargets, BuildTarget(..), readUserBuildTargets)
import Distribution.Verbosity (silent, verbose)
import Distribution.Types.ComponentName
import Distribution.Simple.Program.Types (programPath)
import Distribution.Simple.Program.Db (lookupKnownProgram, lookupProgram, knownPrograms)
import Distribution.Simple.Program (Program, gccProgram, arProgram, runDbProgram, simpleProgram, ghcProgram)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import Distribution.Types.HookedBuildInfo
import Data.List (isPrefixOf, isSuffixOf, intercalate)
import System.Environment (getArgs, getProgName)
import Distribution.Simple.LocalBuildInfo (Component (..), withAllComponentsInBuildOrder, componentBuildDir)
import Distribution.Types.TestSuite (TestSuite(..))
import Distribution.Types.TestSuiteInterface (TestSuiteInterface(..) )
import Distribution.Simple.Test.LibV09 (stubName)
import Distribution.Types.Executable (exeName, Executable(..))
import Distribution.Types.Benchmark (Benchmark(..))
import Distribution.Types.TestSuite (TestSuite(..))
import Distribution.Types.UnqualComponentName (unUnqualComponentName, mkUnqualComponentName)


emarProgram :: Program
emarProgram = simpleProgram "emar"

buildEMCCLib :: PackageDescription -> LocalBuildInfo -> IO ()
buildEMCCLib desc lbi = do
    let verbosity = verbose
    -- get build dir
    createDirectoryIfMissingVerbose verbosity True ((buildDir lbi) </> "emcc")
    --
    case library desc of
        Just lib -> do
            -- Let's see if we are going to export anything. If not there is likely no point in compiling anything
            -- from the C code.
            names <- forM (jsSources . libBuildInfo $ lib) $ \src -> do
                unwords . concatMap (drop 2 . words) . filter (isPrefixOf "// EMCC:EXPORTED_FUNCTIONS") . lines <$> readFile src

            unless (null names) $ do
                let depIncludeDirs = concatMap IPI.includeDirs (topologicalOrder $ installedPkgs lbi)
                -- alright, let's compile all .c files into .o files with emcc, which is the `gcc` program.
                forM_ (cSources . libBuildInfo $ lib) $ \src -> do
                    let dst = (buildDir lbi) </> "emcc" </> (src -<.> "o")
                    createDirectoryIfMissingVerbose verbosity True (takeDirectory dst)
                    runDbProgram verbosity gccProgram (withPrograms lbi) $
                        ["-c", src, "-o", dst] ++ ["-I" <> incDir | incDir <- (includeDirs . libBuildInfo $ lib) ++ depIncludeDirs]

                -- and now construct a canonical `.js_a` file, *if* we have any cSources we turned into objects.
                unless (null . cSources . libBuildInfo $ lib) $ do
                    let dstLib = (buildDir lbi) </> "libEMCC" <> (unPackageName . pkgName . package $ desc) <> ".js_a"
                    runDbProgram verbosity emarProgram (withPrograms lbi) $
                        [ "-r",  dstLib ] ++ [ (buildDir lbi) </> "emcc" </> (src -<.> "o") | src <- cSources . libBuildInfo $ lib ]

                let expLib = (buildDir lbi) </> "libEMCC" <> (unPackageName . pkgName . package $ desc) <> ".exported.js_a"
                writeFile expLib (unwords names)

        -- if there's no lib, this is a fairly pointless exercise
        Nothing -> return ()

-- This is here so that we can link multiple libEMCC* libraries fromd ependencies together with emcc.
-- however we don't have figured out how to get the EXPORTED_FUNCTIONS from each dependency merged yet.
--
linkEMCCLib :: PackageDescription -> LocalBuildInfo -> IO ()
linkEMCCLib desc lbi = linkCLib ("emcc" </> "lib.js") desc lbi

linkEMCCTHLib :: PackageDescription -> LocalBuildInfo -> IO ()
linkEMCCTHLib desc lbi = linkCLib ("th-support.js") desc lbi

linkCLib :: String -> PackageDescription -> LocalBuildInfo -> IO ()
linkCLib libname desc lbi = do
    withAllComponentsInBuildOrder desc lbi $ \comp clbi -> do
        let extraLibs = [ "-l" <> l | l <- concatMap IPI.extraLibraries (topologicalOrder $ installedPkgs lbi)
                                    , l /= "m"
                                    , l /= "dl" ]
            libDirs = [ "-L" <> path | path <- concatMap IPI.libraryDirs (topologicalOrder $ installedPkgs lbi) ]

        let verbosity = verbose
        libs <- filterM doesFileExist $
                concatMap (\x -> [ libDir </> "libEMCC" <> (unPackageName . pkgName . sourcePackageId $ x) <> ".js_a"
                                | libDir <- libraryDirs x ])
                        (topologicalOrder $ installedPkgs lbi)
        exff <- filterM doesFileExist $
                concatMap (\x -> [ libDir </> "libEMCC" <> (unPackageName . pkgName . sourcePackageId $ x) <> ".exported.js_a"
                                | libDir <- libraryDirs x ])
                        (topologicalOrder $ installedPkgs lbi)
        print exff
        exfns <- concat <$> forM exff (fmap words . readFile)
        unless (null libs && null exfns) $ do
            libs <- case libs of
                [] -> do writeFile (buildDir lbi </> "emcc_linking_dummy.c") ""
                         runDbProgram verbosity gccProgram (withPrograms lbi) $
                            ["-c", buildDir lbi </> "emcc_linking_dummy.c", "-o", buildDir lbi </> "emcc_linking_dummy.o"]
                         return [(buildDir lbi </> "emcc_linking_dummy.o")]
                _ -> return libs

            let dst = if libname == "emcc" </> "lib.js" then buildDir lbi
                      -- who designed this shit in cabal?
                      else case comp of
                          (CTest test@(TestSuite { testInterface = TestSuiteLibV09 _ _ })) -> buildDir lbi </> stubName test </> stubName test ++ "-tmp"
                          (CTest test@(TestSuite { testInterface = TestSuiteExeV10 _ _ })) -> buildDir lbi </> unUnqualComponentName (testName test) </> unUnqualComponentName (testName test) ++ "-tmp"
                          (CExe exe) -> buildDir lbi </> unUnqualComponentName (exeName exe) </> unUnqualComponentName (exeName exe) ++ "-tmp"
                          _ -> componentBuildDir lbi clbi
                dst' = dst </> libname
            createDirectoryIfMissingVerbose verbosity True (takeDirectory dst')
            runDbProgram verbosity gccProgram (withPrograms lbi) $
                [ "-o", dst'
                , "-s", "WASM=0"
                , "-s", "ALLOW_TABLE_GROWTH" -- we need this for addFunction/removeFunction
                -- addFunction, removeFunction are for dynamic functions.
                -- getTempRet0/setTempRet0 are for 64bit legalization.
                , "-s", "EXPORTED_RUNTIME_METHODS=['printErr','addFunction','removeFunction','getTempRet0','setTempRet0']"
                --
                , "-s", "EXPORTED_FUNCTIONS=[" <> intercalate ", " (map (\f -> "'" <> f <> "'") exfns) <> "]"
                ] ++ libs ++ libDirs ++ extraLibs

postBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postBuildHook args flags desc lbi = do
    case (takeFileName . programPath <$> lookupProgram ghcProgram (withPrograms lbi)) of
        Just "js-unknown-ghcjs-ghc" ->
            readBuildTargets silent desc (buildArgs flags) >>= \case
                [BuildTargetComponent (CLibName _)] -> print "OK. Lib (Build)" >> buildEMCCLib desc lbi
                [BuildTargetComponent (CExeName _)] -> print "OK. Exe"
                [BuildTargetComponent (CTestName _)] -> print "OK. Test"
                [BuildTargetComponent (CBenchName _)] -> print "OK. Bench"
                _ -> print "EEk!"
        _ -> return ()

postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postConfHook args flags desc lbi = do
    case (takeFileName . programPath <$> lookupProgram ghcProgram (withPrograms lbi)) of
        Just "js-unknown-ghcjs-ghc" -> do
            -- always link the TH lib
            -- this is technically only needed if the package uses TH somewhere.
            linkEMCCTHLib desc lbi
            -- only link the final lib if we want to produce an output.
            readBuildTargets silent desc (configArgs flags) >>= \case
                [BuildTargetComponent (CLibName _)] -> print "OK. Lib" >> postConf simpleUserHooks args flags desc lbi
                [BuildTargetComponent (CExeName _)] -> print "OK. Exe (Link)" >> linkEMCCLib desc lbi
                [BuildTargetComponent (CTestName _)] -> print "OK. Test (Link)" >> linkEMCCLib desc lbi
                [BuildTargetComponent (CBenchName _)] -> print "OK. Bench (Link)" >> linkEMCCLib desc lbi
                _ -> print "EEk!"
        -- defer to default postConf. XXX we should do this for the above cases in linkEMCCLib as well!
        _ -> postConf simpleUserHooks args flags desc lbi

--
-- Injecting emcc/lib.js as needed.
--
-- We inject jsSources: dist/build/emcc/lib.js, the amalgamated emcc library,
-- into Executables, Tests and Benchmarks.
emccBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
emccBuildHook desc lbi hooks flags = do
    let lbi' = lbi { localPkgDescr = updatePackageDescription (localPkgDescr lbi) }
        -- for some reason tests/benchmarks seem to rely on the description, whereas libraries and executables depend on the local build info...
        desc' = updatePackageDescription desc
    doesFileExist jsLib >>= \x -> print $ jsLib <> " " <> (if x then "exists" else "doesn't exist!")
    doesFileExist jsLib >>= \case
        True ->  buildHook simpleUserHooks desc' lbi' hooks flags
        False -> buildHook simpleUserHooks desc  lbi  hooks flags

  where
    jsLib = "dist/build" </> "emcc" </> "lib.js"
    extraOpts = PerCompilerFlavor [jsLib] []
    -- don't inject it for libraries, only for exe, test, bench.
    updateLibrary :: Library -> Library
    updateLibrary = id -- lib@Library{ libBuildInfo = bi } = lib { libBuildInfo = bi { options = options bi <> extraOpts } }
    updateExe :: Executable -> Executable
    updateExe exe@Executable{ buildInfo = bi } = exe { buildInfo = bi { options = options bi <> extraOpts } }
    updateTest :: TestSuite -> TestSuite
    updateTest test@TestSuite{ testBuildInfo = bi } = test { testBuildInfo = bi { options = options bi <> extraOpts } }
    updateBench :: Benchmark -> Benchmark
    updateBench bench@Benchmark{ benchmarkBuildInfo = bi } = bench { benchmarkBuildInfo = bi { options = options bi <> extraOpts } }
    updatePackageDescription :: PackageDescription -> PackageDescription
    updatePackageDescription desc = desc
        { library = updateLibrary <$> library desc
        , executables = updateExe <$> executables desc
        , testSuites = updateTest <$> testSuites desc
        , benchmarks = updateBench <$> benchmarks desc
        }

--
-- Injecting EMCC<name> extra libraries as needed
-- this one we only need alongside library components; as it's shipped and installed
-- into the package databse. This is not necesary for executables/test/benchmarks, that
-- are not installed into the package database
--
emccCopyHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
emccCopyHook desc lbi hooks flags = do
    emccLibs <- filterM (\l -> doesFileExist (buildDir lbi </> "lib" <> l <> ".js_a"))
                        [ "EMCC" <> (unPackageName . pkgName . package $ desc)
                        , "EMCC" <> (unPackageName . pkgName . package $ desc) <> ".exported" ]
    print $ "EMCC extra lib files: " ++ intercalate ", " emccLibs
    let lbi' = lbi { localPkgDescr = updatePackageDescription emccLibs (localPkgDescr lbi) }
        desc' = updatePackageDescription emccLibs desc
    copyHook simpleUserHooks desc' lbi' hooks flags
  where
    emccLib = (buildDir lbi) </> "libEMCC" <> (unPackageName . pkgName . package $ desc) <> ".js_a"
    -- don't inject it for libraries, only for exe, test, bench.
    updateLibrary :: [String] -> Library -> Library
    updateLibrary extraLibs lib@Library{ libBuildInfo = bi } = lib { libBuildInfo = bi { extraBundledLibs = extraBundledLibs bi <> extraLibs } }
    updatePackageDescription :: [String] -> PackageDescription -> PackageDescription
    updatePackageDescription extraLibs desc = desc { library = updateLibrary extraLibs <$> library desc }

emccRegHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
emccRegHook desc lbi hooks flags = do
    emccLibs <- filterM (\l -> doesFileExist (buildDir lbi </> "lib" <> l <> ".js_a"))
                        [ "EMCC" <> (unPackageName . pkgName . package $ desc)
                        , "EMCC" <> (unPackageName . pkgName . package $ desc) <> ".exported" ]
    print $ "EMCC extra lib files: " ++ intercalate ", " emccLibs
    let lbi' = lbi { localPkgDescr = updatePackageDescription emccLibs (localPkgDescr lbi) }
        desc' = updatePackageDescription emccLibs desc
    regHook simpleUserHooks desc' lbi' hooks flags
  where
    emccLib = (buildDir lbi) </> "libEMCC" <> (unPackageName . pkgName . package $ desc) <> ".js_a"
    -- don't inject it for libraries, only for exe, test, bench.
    updateLibrary :: [String] -> Library -> Library
    updateLibrary extraLibs lib@Library{ libBuildInfo = bi } = lib { libBuildInfo = bi { extraBundledLibs = extraBundledLibs bi <> extraLibs } }
    updatePackageDescription :: [String] -> PackageDescription -> PackageDescription
    updatePackageDescription extraLibs desc = desc { library = updateLibrary extraLibs <$> library desc }

emccUnregHook :: PackageDescription -> LocalBuildInfo -> UserHooks -> RegisterFlags -> IO ()
emccUnregHook desc lbi hooks flags = do
    emccLibs <- filterM (\l -> doesFileExist (buildDir lbi </> "lib" <> l <> ".js_a"))
                        [ "EMCC" <> (unPackageName . pkgName . package $ desc)
                        , "EMCC" <> (unPackageName . pkgName . package $ desc) <> ".exported" ]
    print $ "EMCC extra lib files: " ++ intercalate ", " emccLibs
    let lbi' = lbi { localPkgDescr = updatePackageDescription emccLibs (localPkgDescr lbi) }
        desc' = updatePackageDescription emccLibs desc
    unregHook simpleUserHooks desc' lbi' hooks flags
  where
    emccLib = (buildDir lbi) </> "libEMCC" <> (unPackageName . pkgName . package $ desc) <> ".js_a"
    -- don't inject it for libraries, only for exe, test, bench.
    updateLibrary :: [String] -> Library -> Library
    updateLibrary extraLibs lib@Library{ libBuildInfo = bi } = lib { libBuildInfo = bi { extraBundledLibs = extraBundledLibs bi <> extraLibs } }
    updatePackageDescription :: [String] -> PackageDescription -> PackageDescription
    updatePackageDescription extraLibs desc = desc { library = updateLibrary extraLibs <$> library desc }
--
-- Main
--
main :: IO ()
main = do
    args <- getArgs
    defaultMainWithHooksArgs emccHooks (injectEmar args)
  where
    injectEmar :: [String] -> [String]
    injectEmar [] = []
    injectEmar (x:xs) | "--with-gcc=" `isPrefixOf` x
                      , "emcc" `isSuffixOf` x
        = x:("--with-emar="<> (takeDirectory $ drop 11 $ x) </> "emar"):injectEmar xs
    injectEmar (x:xs) = x:injectEmar xs

    emccHooks :: UserHooks
    emccHooks = simpleUserHooks
        { postConf  = postConfHook
        , buildHook = emccBuildHook
        , postBuild = postBuildHook
        , copyHook  = emccCopyHook
        , regHook   = emccRegHook
        , unregHook = emccUnregHook
        , hookedPrograms = [emarProgram]
        }
