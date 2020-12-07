{ mkDerivation, aeson, async, base, bytestring, Cabal, cmdargs
, containers, curl, directory, directory-tree, fetchgit, hpc, HUnit
, process, pureMD5, regex-posix, retry, safe, split, stdenv
, transformers
}:
mkDerivation {
  pname = "hpc-coveralls";
  version = "1.2.0";
  src = fetchgit {
    url = "https://github.com/sevanspowell/hpc-coveralls.git";
    sha256 = "02dmcmqc845s7sdgdnk3xxn7l6jj8faa7547b4cii9mgv09arspj";
    rev = "14df0f7d229f4cd2e79f8eabb1a740097fdfa430";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring Cabal cmdargs containers curl directory
    directory-tree hpc process pureMD5 retry safe split transformers
  ];
  executableHaskellDepends = [
    aeson async base bytestring Cabal cmdargs containers curl directory
    directory-tree hpc process pureMD5 regex-posix retry safe split
    transformers
  ];
  testHaskellDepends = [ base HUnit ];
  jailbreak = true;
  homepage = "https://github.com/guillaume-nargeot/hpc-coveralls";
  description = "Coveralls.io support for Haskell.";
  license = stdenv.lib.licenses.bsd3;
}
