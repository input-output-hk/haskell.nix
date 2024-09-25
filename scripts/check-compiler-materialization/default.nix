# This is for checking the materialization of new GHC versions
# See docs/adding-new-ghc.md for more details.
{ compiler-nix-name ? throw "Please pass `compiler-nix-name` or `compiler-nix-names`", compiler-nix-names ? [ compiler-nix-name ], systems ? [ builtins.currentSystem ] }:
let
  eval = (import ../../. {}).pkgs;
in builtins.listToAttrs (builtins.concatMap (system: builtins.concatMap (compiler-nix-name:
  let haskellNix = import ../../. { checkMaterialization = true; inherit system; };
      pkgs = haskellNix.pkgs-unstable;
      inherit (pkgs.haskell-nix.compiler.${compiler-nix-name}) version;
      # Older versions of GHC do not work with the latest nixpkgs.
      pkgsForWindows =
        if __compareVersions version "8.10" >= 0
          then pkgs
          else import haskellNix.sources.nixpkgs-2105
              (haskellNix.nixpkgsArgs // { localSystem = { inherit system; }; });
      prefix = "${system}-${compiler-nix-name}";
  in ([
  # This set of derivations should be enough to ensure all the materialized files for a
  # given GHC version are checked.
  { name = "${prefix}-extra";         value = (pkgs.haskell-nix.roots' compiler-nix-name).ghc-extra-projects-nix or {}; }
  { name = "${prefix}-boot";          value = pkgs.ghc-boot-packages-nix.${compiler-nix-name}; }
  { name = "${prefix}-iserv";         value = pkgs.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy.project.plan-nix; }
  { name = "${prefix}-iserv-int";     value = pkgs.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy-interpreter.project.plan-nix; }
  { name = "${prefix}-hello";         value = (pkgs.haskell-nix.tool compiler-nix-name "hello" {}).project.plan-nix; }
] ++ eval.lib.optionals (!__elem system ["aarch64-darwin" "aarch64-linux" "x86_64-darwin"]) [
  { name = "${prefix}-windows";            value = (pkgsForWindows.pkgsCross.mingwW64.haskell-nix.roots' compiler-nix-name).ghc-extra-projects-nix or {}; }
  { name = "${prefix}-iserv-windows";      value = pkgsForWindows.pkgsCross.mingwW64.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy.project.plan-nix; }
  { name = "${prefix}-iserv-int-windows";  value = pkgsForWindows.pkgsCross.mingwW64.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy-interpreter.project.plan-nix; }
  { name = "${prefix}-hello-windows";      value = (pkgsForWindows.pkgsCross.mingwW64.haskell-nix.tool compiler-nix-name "hello" {}).project.plan-nix; }
] ++ eval.lib.optionals (system == "x86_64-linux") [
  # In some cased you may need comment out one or more of these if the GHC version needed cannot be built.
  { name = "${prefix}-musl";               value = (pkgs.pkgsCross.musl64.haskell-nix.roots' compiler-nix-name).ghc-extra-projects-nix or {}; }
  { name = "${prefix}-iserv-musl";         value = pkgs.pkgsCross.musl64.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy.project.plan-nix; }
  { name = "${prefix}-iserv-int-musl";     value = pkgs.pkgsCross.musl64.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy-interpreter.project.plan-nix; }
  { name = "${prefix}-hello-musl";         value = (pkgs.pkgsCross.musl64.haskell-nix.tool compiler-nix-name "hello" {}).project.plan-nix; }
] ++ eval.lib.optionals (system == "x86_64-linux" && !__elem compiler-nix-name ["ghc901" "ghc921" "ghc941" "ghc942" "ghc943"]) [
  { name = "${prefix}-arm";                value = (pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.roots' compiler-nix-name).ghc-extra-projects-nix or {}; }
  { name = "${prefix}-iserv-arm";          value = pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy.project.plan-nix; }
  { name = "${prefix}-iserv-int-arm";      value = pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy-interpreter.project.plan-nix; }
  { name = "${prefix}-hello-arm";          value = (pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.tool compiler-nix-name "hello" {}).project.plan-nix; }
] ++ eval.lib.optionals (
        (system == "aarch64-linux" && !__elem compiler-nix-name ["ghc8107" "ghc901" "ghc902" "ghc921" "ghc922" "ghc923" "ghc924" "ghc925" "ghc926" "ghc941" "ghc942" "ghc943"])
     || (system == "x86_64-linux"  && !__elem compiler-nix-name ["ghc8107" "ghc901" "ghc902"
     "ghc921" "ghc922" "ghc923" "ghc924" "ghc925" "ghc926" "ghc941" "ghc942" "ghc943" "ghc944" "ghc945" "ghc947" "ghc961" "ghc962" "ghc963" "ghc964" "ghc965" "ghc9820230704"])) [
  { name = "${prefix}-arm-musl";           value = (pkgs.pkgsCross.aarch64-multiplatform-musl.haskell-nix.roots' compiler-nix-name).ghc-extra-projects-nix or {}; }
  { name = "${prefix}-iserv-arm-musl";     value = pkgs.pkgsCross.aarch64-multiplatform-musl.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy.project.plan-nix; }
  { name = "${prefix}-iserv-int-arm-musl"; value = pkgs.pkgsCross.aarch64-multiplatform-musl.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy-interpreter.project.plan-nix; }
  { name = "${prefix}-hello-arm-musl";     value = (pkgs.pkgsCross.aarch64-multiplatform-musl.haskell-nix.tool compiler-nix-name "hello" {}).project.plan-nix; }
] ++ eval.lib.optionals (
        (system == "x86_64-linux"   && __elem compiler-nix-name ["ghc8107" "ghc961" "ghc962" "ghc963" "ghc964" "ghc965" "ghc981" "ghc982" "ghc9820230704"])
     || (system == "aarch64-linux"  && __elem compiler-nix-name ["ghc961" "ghc962" "ghc963" "ghc964" "ghc965" "ghc981" "ghc982" "ghc9820230704"])
     || (system == "x86_64-darwin"  && __elem compiler-nix-name ["ghc8107" "ghc961" "ghc962" "ghc963" "ghc964" "ghc964" "ghc981" "ghc982" "ghc9820230704"])
     || (system == "aarch64-darwin" && __elem compiler-nix-name ["ghc961" "ghc962" "ghc963" "ghc964" "ghc981" "ghc965" "ghc982" "ghc9820230704"])) [
  { name = "${prefix}-boot-ghcjs";         value = pkgs.pkgsCross.ghcjs.ghc-boot-packages-nix.${compiler-nix-name}; }
  { name = "${prefix}-ghcjs";              value = (pkgs.pkgsCross.ghcjs.haskell-nix.roots' compiler-nix-name).ghc-extra-projects-nix or {}; }
  { name = "${prefix}-hello-ghcjs";        value = (pkgs.pkgsCross.ghcjs.haskell-nix.tool compiler-nix-name "hello" {}).project.plan-nix; }
])) compiler-nix-names) systems)
