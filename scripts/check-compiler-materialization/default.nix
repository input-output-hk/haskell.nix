# This is for checking the materialization of new GHC versions
# See docs/adding-new-ghc.md for more details.
{ compiler-nix-name, systems ? [ builtins.currentSystem ] }:
let
  eval = (import ../../. {}).pkgs;
in eval.linkFarm "check-${compiler-nix-name}" (builtins.concatMap (system:
  let haskellNix = import ../../. { checkMaterialization = true; inherit system; };
      pkgs = haskellNix.pkgs-unstable;
      inherit (pkgs.haskell-nix.compiler.${compiler-nix-name}) version;
      # Older versions of GHC do not work with the latest nixpkgs.
      pkgsForWindows =
        if __compareVersions version "8.10" >= 0
          then pkgs
          else import haskellNix.sources.nixpkgs-2105
              (haskellNix.nixpkgsArgs // { localSystem = { inherit system; }; });
  in [
  # This set of derivations should be enough to ensure all the materialized files for a
  # given GHC version are checked.
  { name = "${system}-cabal-install"; path = pkgs.haskell-nix.cabal-install.${compiler-nix-name}; }
  { name = "${system}-nix-tools";     path = pkgs.haskell-nix.nix-tools.${compiler-nix-name}; }
  { name = "${system}-extra";         path = pkgs.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "${system}-boot";          path = pkgs.ghc-boot-packages-nix.${compiler-nix-name}; }
] ++ eval.lib.optionals (system == "x86_64-linux") ([
  # In some cased you may need comment out one or more of these if the GHC version needed cannot be built.
  { name = "${system}-musl";          path = pkgs.pkgsCross.musl64.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
] ++ eval.lib.optionals (!__elem compiler-nix-name ["ghc901" "ghc902" "ghc921" "ghc922"]) [
  { name = "${system}-windows";       path = pkgsForWindows.pkgsCross.mingwW64.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
] ++ eval.lib.optionals (__elem compiler-nix-name ["ghc884" "ghc8105" "ghc8106" "ghc8107"]) [
  { name = "${system}-arm";           path = pkgs.pkgsCross.aarch64-multiplatform.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
]) ++ eval.lib.optionals (__elem compiler-nix-name ["ghc865" "ghc884" "ghc8105" "ghc8106" "ghc8107"] && system != "aarch64-darwin") [
  { name = "${system}-ghcjs";         path = pkgs.pkgsCross.ghcjs.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
]) systems)

