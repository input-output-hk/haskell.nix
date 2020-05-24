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
    in if p == "" || p == "/"
      then ""
      else if lib.hasSuffix "/" p
        then p
        else p + "/";
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
            # This is a handy way to find out why different files are included
            # traceReason = reason: v: if v then builtins.trace (rPath + " : " + reason) true else false;
            traceReason = reason: v: v;
          in
            traceReason "directory is needed" (
              lib.any (d: lib.strings.hasPrefix (rPath + "/") d) (
                   dirsNeeded
                ++ package.licenseFiles
                ++ package.extraSrcFiles
                ++ component.extraSrcFiles
                ++ package.extraDocFiles
                ++ builtins.map (f: dataDir + f) package.dataFiles
                ++ otherSourceFiles))
            || traceReason "cabal package definition" (lib.strings.hasSuffix ".cabal" rPath)
            || traceReason "hpack package defintion" (rPath == "package.yaml")
            || traceReason "data file" (lib.strings.hasPrefix dataDir rPath
              && dataFileMatch rPath)
            || traceReason "haskell source dir" (lib.any (d: lib.strings.hasPrefix d rPath) hsSourceDirs)
            || traceReason "include dir" (lib.any (d: lib.strings.hasPrefix d rPath) includeDirs)
            || traceReason "license file" (licenseMatch rPath)
            || traceReason "extra source file" (extraSrcMatch rPath)
            || traceReason "extra doc file" (extraDocMatch rPath)
            || traceReason "other source file" (lib.any (f: f == rPath) otherSourceFiles);
      }