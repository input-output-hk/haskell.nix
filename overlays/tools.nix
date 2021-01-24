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
    haskell-language-server-wrapper = "haskell-language-server";
  };

  # Packages that are better known by their package name.  We are not
  # reusing toolPackageName here as perhaps the more one package
  # will have the same exe name.
  packageToolName = {
    cabal-install = "cabal";
  };

  hackage-tool = { name, ... }@args':
    let
      args = { caller = "hackage-tool"; } // args';
    in
      (final.haskell-nix.hackage-package
        (args // { name = final.haskell-nix.toolPackageName.${name} or name; }))
          .components.exes."${final.haskell-nix.packageToolName.${name} or name}";

  tool = compiler-nix-name: name: versionOrArgs:
    let
      args' = final.haskell-nix.haskellLib.versionOrArgsToArgs versionOrArgs;
      args = { inherit compiler-nix-name; } // args';
    in
      (if final.haskell-nix.custom-tools ? "${name}"
          && final.haskell-nix.custom-tools."${name}" ? "${args.version}"
        then final.haskell-nix.custom-tools."${name}"."${args.version}"
        else final.haskell-nix.hackage-tool) (args // { inherit name; });

  tools = compiler-nix-name: lib.mapAttrs (final.haskell-nix.tool compiler-nix-name);

  # Like `tools` but allows default ghc to be specified
  toolsForGhc = ghcOverride: toolSet:
    final.haskell-nix.tools (
      lib.mapAttrs (name: versionOrArgs:
        let args = final.haskell-nix.haskellLib.versionOrArgsToArgs versionOrArgs;
        in
          # Add default ghc if not specified in the args
          (lib.optionalAttrs (!(args ? "compiler-nix-name" || args ? "ghc"))
            { inherit ghcOverride; }
          ) // args
      ) toolSet
    );

  # Tools not in hackage yet
  # When adding custom tools here, consider adding them
  # to the `tools` attribute defined in `build.nix` to make
  # sure they are cached.
  custom-tools = {
    # Currently everything we want is in hackage.
    # Before addy anything here consider uploading to hackage instead.
    # If that is not possible look at the git history of this file to see examples
    # of how to add a custom-tool.
  };
}; }
