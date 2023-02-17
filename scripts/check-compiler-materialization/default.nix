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
  in
  # Exclude version that are older than the boot compiler
    eval.lib.optionals (
        (system != "aarch64-linux"  || !__elem compiler-nix-name ["ghc865" "ghc881" "ghc882" "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc810420210212" "ghc8105"])
     && (system != "aarch64-darwin" || !__elem compiler-nix-name ["ghc865" "ghc881" "ghc882" "ghc883" "ghc884" "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc810420210212" "ghc8105" "ghc8106"])
     && (system != "x86_64-darwin" || !__elem compiler-nix-name ["ghc8102" "ghc8103"])) ([
  # This set of derivations should be enough to ensure all the materialized files for a
  # given GHC version are checked.
  { name = "${prefix}-cabal-install"; value = pkgs.haskell-nix.cabal-install.${compiler-nix-name}; }
  { name = "${prefix}-nix-tools";     value = pkgs.haskell-nix.nix-tools.${compiler-nix-name}; }
  { name = "${prefix}-extra";         value = pkgs.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "${prefix}-boot";          value = pkgs.ghc-boot-packages-nix.${compiler-nix-name}; }
  { name = "${prefix}-iserv";         value = pkgs.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy.project.plan-nix; }
  { name = "${prefix}-iserv-int";     value = pkgs.haskell-nix.iserv-proxy-exes.${compiler-nix-name}.iserv-proxy-interpreter.project.plan-nix; }
  { name = "${prefix}-hello";         value = pkgs.haskell-nix.tool compiler-nix-name "hello" {}; }
] ++ eval.lib.optionals (!__elem system ["aarch64-darwin" "aarch64-linux"]
         && !__elem compiler-nix-name ["ghc865" "ghc881" "ghc882" "ghc883"]) [
  { name = "${prefix}-windows";       value = pkgsForWindows.pkgsCross.mingwW64.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "${prefix}-hello-windows"; value = pkgsForWindows.pkgsCross.mingwW64.haskell-nix.tool compiler-nix-name "hello" {}; }
] ++ eval.lib.optionals (system == "x86_64-linux"
         && !__elem compiler-nix-name ["ghc881"]) [
  # In some cased you may need comment out one or more of these if the GHC version needed cannot be built.
  { name = "${prefix}-musl";          value = pkgs.pkgsCross.musl64.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "${prefix}-hello-musl";    value = pkgs.pkgsCross.musl64.haskell-nix.tool compiler-nix-name "hello" {}; }
] ++ eval.lib.optionals (system == "x86_64-linux" && __elem compiler-nix-name ["ghc884" "ghc8105" "ghc8106" "ghc8107" "ghc902" "ghc922" "ghc923" "ghc924" "ghc925"]) [
  { name = "${prefix}-arm";           value = pkgs.pkgsCross.aarch64-multiplatform.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
] ++ eval.lib.optionals (system == "x86_64-linux" && __elem compiler-nix-name ["ghc884" "ghc8106" "ghc8107" "ghc8107" "ghc902" "ghc922" "ghc923" "ghc924" "ghc925"]) [
  { name = "${prefix}-hello-arm";     value = pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.tool compiler-nix-name "hello" {}; }
] ++ eval.lib.optionals (
        (system == "x86_64-linux"  && __elem compiler-nix-name ["ghc865" "ghc884" "ghc8105" "ghc8106" "ghc8107"])
     || (system == "x86_64-darwin" && __elem compiler-nix-name ["ghc8107"])) [
  { name = "${prefix}-ghcjs";         value = pkgs.pkgsCross.ghcjs.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "${prefix}-hello-ghcjs";   value = pkgs.pkgsCross.ghcjs.haskell-nix.tool compiler-nix-name "hello" {}; }
])) compiler-nix-names) systems)

