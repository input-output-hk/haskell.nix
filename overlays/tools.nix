# This overlay helps accessing common executable components.
# Typically we want to make these available in a nix-shell
# created with shellFor.  In most cases the package name
# will be the same as the executable, but we have a
# `toolPackageName` and `packageToolName` mapping to help
# when it is not.
#
# To get a single tool:
#   haskell-nix.tool "ghc884" "cabal" "3.2.0.0"
#
# This does the same thing as:
#   (haskell-nix.hackage-package {
#     compiler-nix-name = "ghc884";
#     name = "cabal-install"
#     version = "3.2.0.0"
#   }).components.exes.cabal
#
# To get an attr set containing multiple tools:
#   haskell-nix.tools "ghc884" { cabal = "3.2.0.0"; hlint = "2.2.11"; }
#
# To add tools to a shell:
#   shellFor { tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; }; }
#
# When used in shellFor the tools will be compiled with the same version
# of ghc used in the shell (the build ghc in the case of cross compilation).
#
# To get tools for use with project `p` without using shellFor:
#   p.tool "cabal" "3.2.0.0"
#   p.tools { cabal = "3.2.0.0"; hlint = "2.2.11"; }
# (the ghc version used to build it will match the one used in the project)
#
# Instead of a version string we can use an attr set containing
# arguments that will be passed to `cabalProject`.
#
# For instance to add haskell.nix modules. Use:
#   haskell-nix.tool "ghc884" "cabal" {
#      version = "3.2.0.0";
#      modules = [ ... ];
#   }
#
final: prev:
let
  inherit (final) lib;

in { haskell-nix = prev.haskell-nix // {

  # Some times the package name in hackage is not the same as tool name.
  # Tools better known by their exe name.
  toolPackageName = {
    cabal = "cabal-install";
    sos = "steeloverseer";
    gen-hie = "implicit-hie";
    refactor = "apply-refact";
  };

  # Packages that are better known by their package name.  We are not
  # reusing toolPackageName here as perhaps the more one package
  # will have the same exe name.
  packageToolName = {
    cabal-install = "cabal";
    steeloverseer = "sos";
    implicit-hie = "gen-hie";
    apply-refact = "refactor";
  };

  hackage-tool = projectModules:
    let
      package = final.haskell-nix.hackage-package (projectModules ++ [
          ({lib, ...}: {
            options.name = lib.mkOption {
              apply = n: final.haskell-nix.toolPackageName.${n} or n;
            };
            config = {
              # Disable benchmarks and tests by default (since we only want the exe component)
              configureArgs = "--disable-benchmarks --disable-tests";
            };
          })
        ]);
      name = package.project.args.name;
      exeName = final.haskell-nix.packageToolName.${name} or name;
    in package.getComponent "exe:${exeName}";

  tool = compiler-nix-name: name: versionOrMod:
      final.haskell-nix.hackage-tool (
           final.haskell-nix.haskellLib.versionOrModToMods versionOrMod
        ++ [(lib.mapAttrs (_: lib.mkOverride 1100) { inherit compiler-nix-name name; })]
      );

  # tool with a default evalPackages to use.
  tool' = evalPackages: compiler-nix-name: name: versionOrMod:
      final.haskell-nix.hackage-tool (
           final.haskell-nix.haskellLib.versionOrModToMods versionOrMod
        ++ [(lib.mapAttrs (_: lib.mkOverride 1100) { inherit evalPackages compiler-nix-name name; })]
      );

  tools = compiler-nix-name: lib.mapAttrs (final.haskell-nix.tool compiler-nix-name);
  tools' = evalPackages: compiler-nix-name: lib.mapAttrs (final.haskell-nix.tool' evalPackages compiler-nix-name);
}; }
