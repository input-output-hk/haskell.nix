# This is for checking the materialization of new GHC versions
# See docs/adding-new-ghc.md for more details.
{ compiler-nix-name }:
let
  eval = (import ../../. {}).pkgs;
  linux = (import ../../. { checkMaterialization = true; system = "x86_64-linux"; }).pkgs;
  darwin = (import ../../. { checkMaterialization = true; system = "x86_64-darwin"; }).pkgs;
in eval.linkFarm "check-${compiler-nix-name}" [
  # This set of derivations should be enough to ensure all the materialized files for a
  # given GHC version are checked.
  { name = "linux-cabal-install";  path = linux.haskell-nix.cabal-install.${compiler-nix-name}; }
  # { name = "darwin-cabal-install"; path = darwin.haskell-nix.cabal-install.${compiler-nix-name}; }
  { name = "linux-nix-tools";      path = linux.haskell-nix.nix-tools.${compiler-nix-name}; }
  { name = "linux";                path = linux.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  # In some cased you may need comment out one or more of these if the GHC version needed cannot be built.
  { name = "musl";                 path = linux.pkgsCross.musl64.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "windows";              path = linux.pkgsCross.mingwW64.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
  { name = "arm";                  path = linux.pkgsCross.aarch64-multiplatform.ghc-extra-projects.${compiler-nix-name}.plan-nix; }
]

