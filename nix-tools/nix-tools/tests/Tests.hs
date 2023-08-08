import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import System.Directory (copyFile, createDirectoryIfMissing, removeDirectoryRecursive, withCurrentDirectory)
import System.Directory.Extra (findExecutable, listFiles)
import System.Environment (setEnv)
import System.FilePath (replaceExtension, takeBaseName, takeExtensions, (</>))
import System.IO.Extra (newTempDir)
import System.Process (callCommand)
import Test.Tasty (defaultMain, testGroup, withResource)
import Test.Tasty.Golden.Advanced (goldenTest2)
import Test.Tasty.Providers

main :: IO ()
main = goldenTests >>= defaultMain

goldenTests :: IO TestTree
goldenTests = do
  -- Use a temporary CABAL_DIR
  checkRequiredProgram "make-install-plan"
  checkRequiredProgram "plan-to-nix"

  -- NOTE: we want these paths to be like "tests/golden/test1.project"
  projectFiles <- findFilesWithExtension ".project" "tests/golden"
  return $
    withHackageIndex $
      testGroup "Tests" (map testProject projectFiles)
  where
    withHackageIndex t =
      withResource
        ( do
            (cabalDir, cleanup) <- newTempDir
            setEnv "CABAL_DIR" cabalDir
            callCommand "cabal update -v"
            return cleanup
        )
        id
        (const t)

testProject :: FilePath -> TestTree
testProject projectFile = test
  where
    goldenPlanJsonFile = replaceExtension projectFile ".plan.json"
    goldenPackagesFile = replaceExtension projectFile ".pkgs.nix"

    testName = takeBaseName projectFile
    testWorkDir = replaceExtension projectFile ".workdir"

    testPlanJsonFile = testWorkDir </> "dist-newstyle/cache/plan.json"
    testPackagesFile = testWorkDir </> "plan-nix/pkgs.nix"

    test = goldenTest2 testName readExpected getActual compareResult updateExpected delete
      where
        readExpected = do
          plan <- BS.readFile goldenPlanJsonFile
          pkgs <- BS.readFile goldenPackagesFile
          return (plan, pkgs)

        updateExpected (plan, pkgs) = do
          BS.writeFile goldenPlanJsonFile plan
          BS.writeFile goldenPackagesFile pkgs

        getActual = do
          createDirectoryIfMissing True testWorkDir
          copyFile projectFile (testWorkDir </> "cabal.project")
          withCurrentDirectory testWorkDir $ do
            callCommand "make-install-plan"
            createDirectoryIfMissing True "plan-nix"
            callCommand "plan-to-nix --output plan-nix"
          plan <- BS.readFile testPlanJsonFile
          pkgs <- BS.readFile testPackagesFile
          return (plan, pkgs)

        delete = removeDirectoryRecursive testWorkDir

        compareResult a b = return $ if a == b then Nothing else Just ""

findFilesWithExtension :: String -> FilePath -> IO [FilePath]
findFilesWithExtension ext dir =
  filter ((== ext) . takeExtensions) <$> listFiles dir

checkRequiredProgram :: String -> IO ()
checkRequiredProgram prg =
  findExecutable prg >>= \mpath ->
    when (isNothing mpath) $ fail (prg ++ " is missing")
