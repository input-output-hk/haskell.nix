# Use cleanSourceWith to filter just the files needed for a particular
# component of the package
{ lib, cleanSourceWith }: package: component: src:
let
  srcStr' = src.origSrc or null;
  subDir = if src.origSubDir or "" == ""
    then ""
    else lib.removePrefix "/" src.origSubDir + "/";
  # Remove a directory for each .. part of a path.
  removeDotDots = parts: lib.reverseList (
    builtins.foldl' (a: b:
      if b == ".."
        then builtins.tail a
        else builtins.concatLists [ [b] a ]) [] parts);
  # Transform
  #  "."            -> ""
  #  "./."          -> ""
  #  "./xyz"        -> "xyz"
  #  "../abc"       -> ERROR
  #  "abc/.."       -> ""
  #  "abc/../xyz"   -> "xyz"
  #  "abc/./xyz"    -> "abc/xyz"
  #  "abc/./../xyz" -> "xyz"
  #  "abc/.././xyz" -> "xyz"
  #  "abc/"         -> "abc/"
  normalizeRelativePath = path:
    let
      # Split the path into component parts and remove the empty ones and single dots.
      nonEmptyParts = lib.filter (x: x != "" && x != ".") (lib.splitString "/" path);
    in lib.concatStringsSep "/" (removeDotDots nonEmptyParts)
      # Keep the trailing slash if there was one.
      + (if lib.hasSuffix "/" path then "/" else "");
  isAbsolutePath = path: lib.hasPrefix "/" path;
  isRelativePath = path: !(isAbsolutePath path);
  normalizePath = path:
    (if isAbsolutePath path
      then "/"
      else ""
    ) + normalizeRelativePath path;
  combinePaths = a: b: if isAbsolutePath b
    then b
    else normalizePath (a + "/" + b);
  # Like normalizePath but with a trailing / when needed
  normalizeDir = dir:
    let p = normalizePath dir;
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
        dataDir = combinePaths subDir package.dataDir;
        hsSourceDirs = builtins.map (d: combinePaths subDir d) component.hsSourceDirs
          ++ (if component.hsSourceDirs == [] then [subDir] else []);
        includeDirs = builtins.map (d: combinePaths subDir d) component.includeDirs;
        dirsNeeded = builtins.map (d: combinePaths subDir d) (
             [dataDir]
          ++ hsSourceDirs
          ++ includeDirs
          ++ package.licenseFiles
          ++ package.extraSrcFiles
          ++ component.extraSrcFiles
          ++ package.extraDocFiles
          ++ builtins.map (f: dataDir + f) package.dataFiles
          ++ otherSourceFiles);
        fileMatch = dir: list:
          let
            prefixes = builtins.map (f: combinePaths dir f) (
              lib.lists.remove null (lib.lists.flatten (
                builtins.map (f: builtins.match "([^*]*)[*].*" f) list)));
            exactMatches = builtins.map (f: combinePaths dir f) (
              lib.lists.remove null (lib.lists.flatten (
                builtins.map (f: builtins.match "([^*]*)" f) list)));
          in rPath: lib.any (d: lib.strings.hasPrefix d rPath) prefixes
                || lib.any (d: d == rPath) exactMatches;
        dataFileMatch = fileMatch dataDir package.dataFiles;
        licenseMatch  = fileMatch subDir package.licenseFiles;
        extraSrcMatch = fileMatch subDir (
             package.extraSrcFiles
          ++ component.extraSrcFiles);
        extraDocMatch = fileMatch subDir package.extraDocFiles;
        otherSourceFiles = builtins.map (f: combinePaths subDir f) (
             component.asmSources
          ++ component.cmmSources
          ++ component.cSources
          ++ component.cxxSources
          ++ component.jsSources);
      in cleanSourceWith {
        subDir = lib.removePrefix "/" (src.origSubDir or "");
        includeSiblings = true;
        src = cleanSourceWith {
          src = src.origSrc or src;
          filter = path: type:
            (!(src ? filter) || src.filter path type) && (
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
                lib.any (d: lib.strings.hasPrefix (rPath + "/") d) dirsNeeded)
              || traceReason "cabal package definition" (lib.strings.hasPrefix subDir rPath
                && lib.strings.hasSuffix ".cabal" rPath)
              || traceReason "hpack package defintion" (lib.strings.hasPrefix subDir rPath
                && rPath == "package.yaml")
              || traceReason "data file" (lib.strings.hasPrefix dataDir rPath
                && dataFileMatch rPath)
              || traceReason "haskell source dir" (lib.any (d: lib.strings.hasPrefix d rPath) hsSourceDirs)
              || traceReason "include dir" (lib.any (d: lib.strings.hasPrefix d rPath) includeDirs)
              || traceReason "license file" (licenseMatch rPath)
              || traceReason "extra source file" (extraSrcMatch rPath)
              || traceReason "extra doc file" (extraDocMatch rPath)
              || traceReason "other source file" (lib.any (f: f == rPath) otherSourceFiles)
            );
        };
      }
