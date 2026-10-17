# Exercise the real CI test's availability without instantiating its projects.
let
  lib = {
    versionAtLeast = actual: minimum: builtins.compareVersions actual minimum >= 0;
    versionOlder = actual: maximum: builtins.compareVersions actual maximum < 0;
    optionalAttrs = condition: attrs: if condition then attrs else {};
    recurseIntoAttrs = attrs: attrs;
  };
  select = compiler-nix-name: isLinux: isAndroid: isAarch32: version:
    import ./default.nix {
      inherit lib compiler-nix-name;
      pkgs = throw "selection must not extend a package set";
      stdenv = {
        buildPlatform = { inherit isLinux; };
        hostPlatform = {
          inherit isAndroid isAarch32;
          isGhcjs = false; isWasm = false; isMusl = false;
          isAarch64 = !isAarch32; isx86_32 = false; isWindows = false;
        };
      };
      util = {};
      project' = _: throw "selection must not force a compiler project";
      haskellLib.isCrossHost = true;
      testSrc = _: throw "selection must not force a source project";
      evalPackages = {};
      evalSystem = "aarch64-darwin";
      pkgsBuildBuild.qemu = { inherit version; };
    };
  fixed = select "ghc914-sh" true true true "11.0.1";
  otherCompiler = select "ghc8107" true true true "11.0.1";
  darwin = select "ghc914-sh" false true true "11.0.1";
  old = select "ghc914-sh" true true true "8.2";
  future = select "ghc914-sh" true true true "12.0";
  arm64 = select "ghc914-sh" true true false "11.0.1";
in
assert !fixed.meta.disabled && fixed ? build-profiled;
assert otherCompiler.meta.disabled && !(otherCompiler ? build-profiled);
assert darwin.meta.disabled && !(darwin ? build-profiled);
assert old.meta.disabled && !(old ? build-profiled);
assert future.meta.disabled && !(future ? build-profiled);
assert !arm64.meta.disabled && !(arm64 ? build-profiled);
true
