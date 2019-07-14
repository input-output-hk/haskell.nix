# Use cleanSourceWith to filter just the files needed for a particular
# component of the package
{ lib, cleanSourceWith }: package: component: src:
let
  srcStr' = src.origSrcSubDir or src.origSrc or null;
  # Transform
  #   "."    -> ""
  #   "./."  -> ""
  #  "./xyz" -> "xyz" 
  normalizeRelativePath = rel:
    if rel == "." || rel == "./."
      then ""
      else lib.strings.removePrefix "./" rel;
  # Like normalizeRelativePath but with a trailing / when needed
  normalizeRelativeDir = dir:
    let p = normalizeRelativePath dir;
    in if p == "" then "" else p + "/";
  # globMatch = pat: s: builtins.match ()
in
  if srcStr' == null || package.detailLevel != "FullDetails"
    then src
    else
      let
        srcStr = toString srcStr';
        dataDir = normalizeRelativeDir package.dataDir;
        hsSourceDirs = builtins.map normalizeRelativeDir component.hsSourceDirs
          ++ (if component.hsSourceDirs == [] then [""] else []);
        includeDirs = builtins.map normalizeRelativeDir component.includeDirs;
        dirsNeeded = [dataDir]
          ++ hsSourceDirs
          ++ includeDirs;
        fileMatch = dir: list:
          let
            prefixes = builtins.map (f: dir + f) (
              lib.lists.remove null (lib.lists.flatten (
                builtins.map (f: builtins.match "([^*]*)[*].*" f) list)));
            exactMatches = builtins.map (f: dataDir + f) (
              lib.lists.remove null (lib.lists.flatten (
                builtins.map (f: builtins.match "([^*]*)" f) list)));
          in rPath: lib.any (d: lib.strings.hasPrefix d rPath) prefixes
                || lib.any (d: d == rPath) exactMatches;
        dataFileMatch = fileMatch dataDir package.dataFiles;
        licenseMatch  = fileMatch "" package.licenseFiles;
        extraSrcMatch = fileMatch "" package.extraSrcFiles;
        extraDocMatch = fileMatch "" package.extraDocFiles;
        otherSourceFiles =
             component.asmSources
          ++ component.cmmSources
          ++ component.cSources
          ++ component.cxxSources
          ++ component.jsSources;
      in cleanSourceWith {
        inherit src;
        filter = path: type:
          assert (if !lib.strings.hasPrefix (srcStr + "/") (path + "/")
             then throw ("Unexpected path " + path + " (expected something in " + srcStr + "/)")
             else true);
          let
            srcStrLen = lib.strings.stringLength srcStr;
            rPath = lib.strings.substring (srcStrLen + 1) (lib.strings.stringLength path - srcStrLen - 1) path;
          in
            # Directories we need
              lib.any (d: lib.strings.hasPrefix (rPath + "/") d) (
                   dirsNeeded
                ++ package.extraSrcFiles
                ++ package.extraDocFiles
                ++ builtins.map (f: dataDir + f) package.dataFiles
                ++ otherSourceFiles)
            # Project files
            || lib.strings.hasSuffix ".cabal" rPath
            || rPath == "package.yaml"
            # Data Files
            || (lib.strings.hasPrefix dataDir rPath
              && dataFileMatch rPath)
            # Haskell Source Files
            || lib.any (d: lib.strings.hasPrefix d rPath) hsSourceDirs
            || lib.any (d: lib.strings.hasPrefix d rPath) includeDirs
            # License Files
            || licenseMatch rPath
            # Extra Source Files
            || extraSrcMatch rPath
            # Extra Doc Files
            || extraDocMatch rPath
            # Other source files
            || lib.any (f: f == rPath) otherSourceFiles;
      }

  /*
  in if (builtins.typeOf src) == "path"
    then lib.cleanSourceWith {
      filter = with pkgs.stdenv;
        name: type:
          let
            stringName (toString name)
            baseName = baseNameOf;
            allDirs = # strip "./" from all dirs
        in
            # Anything that is a directory we might need
            lib.any (d: lib.string.hasPrefix (stringName + "/")) allDirs
          ||
            # 
          ! (
          # Filter out cabal build products.
          baseName == "dist" || baseName == "dist-newstyle" ||
          baseName == "cabal.project.local" ||
          lib.hasPrefix ".ghc.environment" baseName ||
          # Filter out stack build products.
          lib.hasPrefix ".stack-work" baseName ||
          # Filter out files which are commonly edited but don't
          # affect the cabal build.
          lib.hasSuffix ".nix" baseName
        );
      src = lib.cleanSource src;
    } else src


-- | List those source files that should be copied with ordinary permissions.
listPackageSourcesOrdinary :: Verbosity
                           -> PackageDescription
                           -> [PPSuffixHandler]
                           -> IO [FilePath]
listPackageSourcesOrdinary verbosity pkg_descr pps =
  fmap concat . sequenceA $
  [
    -- Library sources.
    fmap concat
    . withAllLib $ \Library {
                      exposedModules = modules,
                      signatures     = sigs,
                      libBuildInfo   = libBi
                    } ->
     allSourcesBuildInfo verbosity libBi pps (modules ++ sigs)

    -- Executables sources.
  , fmap concat
    . withAllExe $ \Executable { modulePath = mainPath, buildInfo = exeBi } -> do
       biSrcs  <- allSourcesBuildInfo verbosity exeBi pps []
       mainSrc <- findMainExeFile verbosity exeBi pps mainPath
       return (mainSrc:biSrcs)

    -- Foreign library sources
  , fmap concat
    . withAllFLib $ \flib@(ForeignLib { foreignLibBuildInfo = flibBi }) -> do
       biSrcs   <- allSourcesBuildInfo verbosity flibBi pps []
       defFiles <- mapM (findModDefFile verbosity flibBi pps)
         (foreignLibModDefFile flib)
       return (defFiles ++ biSrcs)

    -- Test suites sources.
  , fmap concat
    . withAllTest $ \t -> do
       let bi  = testBuildInfo t
       case testInterface t of
         TestSuiteExeV10 _ mainPath -> do
           biSrcs <- allSourcesBuildInfo verbosity bi pps []
           srcMainFile <- findMainExeFile verbosity bi pps mainPath
           return (srcMainFile:biSrcs)
         TestSuiteLibV09 _ m ->
           allSourcesBuildInfo verbosity bi pps [m]
         TestSuiteUnsupported tp ->
           die' verbosity $ "Unsupported test suite type: " ++ show tp

    -- Benchmarks sources.
  , fmap concat
    . withAllBenchmark $ \bm -> do
       let  bi = benchmarkBuildInfo bm
       case benchmarkInterface bm of
         BenchmarkExeV10 _ mainPath -> do
           biSrcs <- allSourcesBuildInfo verbosity bi pps []
           srcMainFile <- findMainExeFile verbosity bi pps mainPath
           return (srcMainFile:biSrcs)
         BenchmarkUnsupported tp -> die' verbosity $ "Unsupported benchmark type: "
                                    ++ show tp

    -- Data files.
  , fmap concat
    . for (dataFiles pkg_descr) $ \filename ->
        let srcDataDirRaw = dataDir pkg_descr
            srcDataDir = if null srcDataDirRaw
              then "."
              else srcDataDirRaw
        in fmap (fmap (srcDataDir </>)) $
             matchDirFileGlob verbosity (specVersion pkg_descr) srcDataDir filename

    -- Extra doc files.
  , fmap concat
    . for (extraDocFiles pkg_descr) $ \ filename ->
        matchDirFileGlob verbosity (specVersion pkg_descr) "." filename

    -- License file(s).
  , return (licenseFiles pkg_descr)

    -- Install-include files, without autogen-include files
  , fmap concat
    . withAllLib $ \ l -> do
       let lbi   = libBuildInfo l
           incls = filter (`notElem` autogenIncludes lbi) (installIncludes lbi)
           relincdirs = "." : filter isRelative (includeDirs lbi)
       traverse (fmap snd . findIncludeFile verbosity relincdirs) incls

    -- Setup script, if it exists.
  , fmap (maybe [] (\f -> [f])) $ findSetupFile ""

    -- The .cabal file itself.
  , fmap (\d -> [d]) (defaultPackageDesc verbosity)

  ]
  where
    -- We have to deal with all libs and executables, so we have local
    -- versions of these functions that ignore the 'buildable' attribute:
    withAllLib       action = traverse action (allLibraries pkg_descr)
    withAllFLib      action = traverse action (foreignLibs pkg_descr)
    withAllExe       action = traverse action (executables pkg_descr)
    withAllTest      action = traverse action (testSuites pkg_descr)
    withAllBenchmark action = traverse action (benchmarks pkg_descr)

-- | Given a buildinfo, return the names of all source files.
allSourcesBuildInfo :: Verbosity
                       -> BuildInfo
                       -> [PPSuffixHandler] -- ^ Extra preprocessors
                       -> [ModuleName]      -- ^ Exposed modules
                       -> IO [FilePath]
allSourcesBuildInfo verbosity bi pps modules = do
  let searchDirs = hsSourceDirs bi
  sources <- fmap concat $ sequenceA $
    [ let file = ModuleName.toFilePath module_
      -- NB: *Not* findFileWithExtension, because the same source
      -- file may show up in multiple paths due to a conditional;
      -- we need to package all of them.  See #367.
      in findAllFilesWithExtension suffixes searchDirs file
         >>= nonEmpty (notFound module_) return
    | module_ <- modules ++ otherModules bi ]
  bootFiles <- sequenceA
    [ let file = ModuleName.toFilePath module_
          fileExts = ["hs-boot", "lhs-boot"]
      in findFileWithExtension fileExts (hsSourceDirs bi) file
    | module_ <- modules ++ otherModules bi ]

  return $ sources ++ catMaybes bootFiles ++ cSources bi ++ cxxSources bi ++ jsSources bi

  where
    nonEmpty x _ [] = x
    nonEmpty _ f xs = f xs
    suffixes = ppSuffixes pps ++ ["hs", "lhs", "hsig", "lhsig"]
    notFound m = die' verbosity $ "Error: Could not find module: " ++ prettyShow m
                 ++ " with any suffix: " ++ show suffixes ++ ". If the module "
                 ++ "is autogenerated it should be added to 'autogen-modules'."
*/