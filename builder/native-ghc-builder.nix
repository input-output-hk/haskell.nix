# The native stage2 GHC library stalls in the stage1 compiler under Rosetta.
# Keep this exception separate from the cross-compiler policy: the same
# library derivation completes on a real x86_64 Linux host. Other GHC
# versions, bootstrap compilers, and target platforms retain their policy.
{ isCrossHost, buildPlatform, packageName, packageVersion, compilerName }:
!isCrossHost
&& buildPlatform.isLinux
&& buildPlatform.isx86_64
&& packageName == "ghc"
&& packageVersion == "9.14"
&& compilerName == "ghc914-sh-stage1-compiler"
