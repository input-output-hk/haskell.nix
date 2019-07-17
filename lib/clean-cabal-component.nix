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
        extraSrcMatch = fileMatch "" (
             package.extraSrcFiles
          ++ component.extraSrcFiles);
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
                ++ component.extraSrcFiles
                ++ package.extraDocFiles
                ++ builtins.map (f:
                  dataDir + (if dataDir == "" then "" else "/") + f) package.dataFiles
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