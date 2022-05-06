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
  in
  # Exclude version that are older than the boot compiler
    eval.lib.optionals (
        (system != "aarch64-linux"  || !__elem compiler-nix-name ["ghc865" "ghc881" "ghc882"])
     && (system != "aarch64-darwin" || !__elem compiler-nix-name ["ghc865" "ghc881" "ghc882" "ghc883" "ghc884" "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc8105" "ghc8106"])) ([
  # This set of derivations should be enough to ensure all the materialized files for a
  # given GHC version are checked.
  { name = "${system}-cabal-install"; value = pkgs.haskell-nix.cabal-install.${compiler-nix-name}; }
  { name = "${system}-nix-tools";     value = pkgs.haskell-nix.nix-tools.${compiler-nix-name}; }
  { name = "${system}-extra";         value = pkgs.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "${system}-boot";          value = pkgs.ghc-boot-packages-nix.${compiler-nix-name}; }
  { name = "${system}-hello";         value = pkgs.haskell-nix.tool compiler-nix-name "hello" {}; }
] ++ eval.lib.optionals (!__elem system ["aarch64-darwin" "aarch64-linux"]) [
  { name = "${system}-windows";       value = pkgsForWindows.pkgsCross.mingwW64.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "${system}-hello-windows"; value = pkgsForWindows.pkgsCross.mingwW64.haskell-nix.tool compiler-nix-name "hello" {}; }
] ++ eval.lib.optionals (system == "x86_64-linux") [
  # In some cased you may need comment out one or more of these if the GHC version needed cannot be built.
  { name = "${system}-musl";          value = pkgs.pkgsCross.musl64.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "${system}-hello-musl";    value = pkgs.pkgsCross.musl64.haskell-nix.tool compiler-nix-name "hello" {}; }
] ++ eval.lib.optionals (system == "x86_64-linux" && __elem compiler-nix-name ["ghc884" "ghc8105" "ghc8106" "ghc8107"]) [
  { name = "${system}-arm";           value = pkgs.pkgsCross.aarch64-multiplatform.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
] ++ eval.lib.optionals (system == "x86_64-linux" && __elem compiler-nix-name ["ghc884" "ghc8106" "ghc8107"]) [
  { name = "${system}-hello-arm";     value = pkgs.pkgsCross.aarch64-multiplatform.haskell-nix.tool compiler-nix-name "hello" {}; }
] ++ eval.lib.optionals (!__elem system ["aarch64-darwin" "aarch64-linux"] && __elem compiler-nix-name ["ghc865" "ghc884" "ghc8105" "ghc8106" "ghc8107"]) [
  { name = "${system}-ghcjs";         value = pkgs.pkgsCross.ghcjs.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "${system}-hello-ghcjs";   value = pkgs.pkgsCross.ghcjs.haskell-nix.tool compiler-nix-name "hello" {}; }
])) compiler-nix-names) systems)

