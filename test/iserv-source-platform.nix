# Capture the real iserv project module without planning or building it.
# Source preparation must follow evalSystem even when the build platform differs.
let
  source = builtins.toFile "iserv-source-platform-input" "";
  packageSet = system: {
    stdenv.hostPlatform = { inherit system; };
    applyPatches = args: builtins.derivation {
      inherit (args) name src patches;
      inherit system;
      builder = "/bin/sh";
    };
  };
  evalSets = builtins.mapAttrs (system: _: packageSet system) {
    aarch64-darwin = {};
    x86_64-linux = {};
  };
  producer = buildSystem: evalSystem:
    let
      final = {
        lib.mapAttrs = builtins.mapAttrs;
        stdenv.hostPlatform = { isAndroid = true; isAarch32 = true; };
        pkgsBuildBuild = evalSets.${buildSystem};
        haskell-nix = (import ../overlays/haskell.nix {
          sources.iserv-proxy = source;
        } final {}).haskell-nix // {
          compiler.ghc914-sh = {};
          evalPackages = evalSets;
          cabalProject' = projectModule:
            let
              args = projectModule {
                pkgs = final;
                config.evalPackages = evalSets.${args.evalSystem};
              };
            in { plan-nix = { inherit (args) src; }; };
        };
      };
    in final.haskell-nix.iserv-proxy-exes.ghc914-sh.evalWith.${evalSystem}
      .plan-nixes.iserv-proxy-interpreter.src.system;
  checks = builtins.concatMap (buildSystem:
    map (evalSystem: producer buildSystem evalSystem == evalSystem)
      (builtins.attrNames evalSets)) (builtins.attrNames evalSets);
in {
  passed = assert builtins.all (value: value) checks; true;
}
