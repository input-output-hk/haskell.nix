{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { executable = false; };
    package = {
      specVersion = "2.0";
      identifier = { name = "zip-archive"; version = "0.4.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "jgm@berkeley.edu";
      author = "John MacFarlane";
      homepage = "http://github.com/jgm/zip-archive";
      url = "";
      synopsis = "Library for creating and modifying zip archives.";
      description = "The zip-archive library provides functions for creating, modifying, and\nextracting files from zip archives. The zip archive format is\ndocumented in <http://www.pkware.com/documents/casestudies/APPNOTE.TXT>.\n\nCertain simplifying assumptions are made about the zip archives: in\nparticular, there is no support for strong encryption, zip files that\nspan multiple disks, ZIP64, OS-specific file attributes, or compression\nmethods other than Deflate. However, the library should be able to read\nthe most common zip archives, and the archives it produces should be\nreadable by all standard unzip programs.\n\nArchives are built and extracted in memory, so manipulating large zip\nfiles will consume a lot of memory. If you work with large zip files or\nneed features not supported by this library, a better choice may be\n<http://hackage.haskell.org/package/zip zip>, which uses a\nmemory-efficient streaming approach. However, zip can only read and\nwrite archives inside instances of MonadIO, so zip-archive is a better\nchoice if you want to manipulate zip archives in \"pure\" contexts.\n\nAs an example of the use of the library, a standalone zip archiver and\nextracter is provided in the source distribution.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."pretty" or (errorHandler.buildDepError "pretty"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."binary" or (errorHandler.buildDepError "binary"))
          (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."array" or (errorHandler.buildDepError "array"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
        buildable = true;
        };
      exes = {
        "zip-archive" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."zip-archive" or (errorHandler.buildDepError "zip-archive"))
            ];
          buildable = if flags.executable then true else false;
          };
        };
      tests = {
        "test-zip-archive" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."process" or (errorHandler.buildDepError "process"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."zip-archive" or (errorHandler.buildDepError "zip-archive"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            ] ++ (pkgs.lib).optional (!system.isWindows) (hsPkgs."unix" or (errorHandler.buildDepError "unix"));
          build-tools = [
            (hsPkgs.buildPackages.unzip.components.exes.unzip or (pkgs.buildPackages.unzip or (errorHandler.buildToolDepError "unzip:unzip")))
            ];
          buildable = true;
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/zip-archive-0.4.1.tar.gz";
      sha256 = "c5d5c9976241dcc25b0d8753dc526bb1bfef60f30dee38c53a7ae56e6be9b1b1";
      });
    }