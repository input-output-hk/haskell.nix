let
  needsNative = import ../builder/native-ghc-builder.nix;
  stage2 = {
    isCrossHost = false;
    buildPlatform = { isLinux = true; isx86_64 = true; };
    packageName = "ghc";
    packageVersion = "9.14";
    compilerName = "ghc914-sh-stage1-compiler";
  };
in
assert needsNative stage2;
assert !(needsNative (stage2 // { packageName = "base"; }));
assert !(needsNative (stage2 // { packageVersion = "9.14.1"; }));
assert !(needsNative (stage2 // { compilerName = "ghc-9.10.3"; }));
assert !(needsNative (stage2 // { compilerName = "ghc914-sh"; }));
assert !(needsNative (stage2 // { isCrossHost = true; }));
assert !(needsNative (stage2 // {
  buildPlatform = { isLinux = false; isx86_64 = true; };
}));
assert !(needsNative (stage2 // {
  buildPlatform = { isLinux = true; isx86_64 = false; };
}));
{
  nativeStage2GhcSelected = true;
  otherPackagesVersionsAndCompilersUnchanged = true;
  crossDarwinAndAarch64Unchanged = true;
}
