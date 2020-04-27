# This overlay helps accessing common executable components.
# Typically we want to make these available in a nix-shell
# created with shellFor.  In most cases the package name
# will be the same as the executable, but we have a
# `toolPackageName` mapping to help when it is not.
#
# To get a single tool:
#   haskell-nix.tool "cabal" "3.2.0.0"
#
# This does the same thing as:
#   (haskell-nix.hackage-package {
#     name = "cabal-install"
#     version = "3.2.0.0"
#   }).components.exes.cabal
#
# To get an attr set containing multiple tools:
#   haskell-nix.tools { cabal = "3.2.0.0"; hlint = "2.2.11"; }
#
# To add tools to a shell:
#   shellFor { tools = { cabal = "3.2.0.0"; hlint = "2.2.11"; }; }
#
# When used in shellFor the tools will be compiled with the same version
# of ghc used in the shell (the build ghc in the case of cross compilation).
#
# Instead of a version string we can use an attr set containing
# arguments that will be passed to `cabalProject`.
#
# For instance to specify the ghc used to compile. Use:
#   haskell-nix.tool "cabal" {
#      version = "3.2.0.0";
#      ghc = haskell-nix.compiler.ghc883;
#   }
#
self: super:
let
  inherit (self) lib;

in { haskell-nix = super.haskell-nix // {

  # Some times the package name in hackage is not the same as tool name.
  toolPackageName = {
    cabal = "cabal-install";
  };

  hackage-tool = { name, ... }@args:
    (self.haskell-nix.hackage-package
      (args // { name = self.haskell-nix.toolPackageName."${name}" or name; }))
        .components.exes."${name}";

  tool = name: versionOrArgs:
    let
      args = self.haskell-nix.haskellLib.versionOrArgsToArgs versionOrArgs;
    in
      (if self.haskell-nix.custom-tools ? "${name}"
          && self.haskell-nix.custom-tools."${name}" ? "${args.version}"
        then self.haskell-nix.custom-tools."${name}"."${args.version}"
        else self.haskell-nix.hackage-tool) (args // { inherit name; });

  tools = lib.mapAttrs self.haskell-nix.tool;

  # Like `tools` but allows default ghc to be specified
  toolsForGhc = ghc: toolSet:
    self.haskell-nix.tools (
      lib.mapAttrs (name: versionOrArgs:
        # Add default ghc if not specified in the args
        { inherit ghc; }
          // self.haskell-nix.haskellLib.versionOrArgsToArgs versionOrArgs
      ) toolSet
    );

  # Tools not in hackage yet
  custom-tools = {
    ghcide.object-code = args:
        (self.haskell-nix.cabalProject (args // {
          name = "ghcide";
          src = self.fetchFromGitHub {
            owner = "mpickering";
            repo = "ghcide";
            rev = "706c59c97c25c66798815c1dc3ee6885a298918a";
            sha256 = "0d158xifwvz0y69ah98ckxakzqpz229mq7rpf2bpbmwhnpw3jmm6";
          };
          modules = [({config, ...}: {
            packages.ghcide.configureFlags = [ "--enable-executable-dynamic" ];
            nonReinstallablePkgs = [ "Cabal" "array" "base" "binary" "bytestring" "containers" "deepseq"
                                     "directory" "filepath" "ghc" "ghc-boot" "ghc-boot-th" "ghc-compact"
                                     "ghc-heap" "ghc-prim" "ghci" "haskeline" "hpc" "integer-gmp"
                                     "libiserv" "mtl" "parsec" "pretty" "process" "rts" "stm"
                                     "template-haskell" "terminfo" "text" "time" "transformers" "unix"
                                     "xhtml"
                                   ];
          })];
          pkg-def-extras = [
                 (hackage: {
              packages = {
                "alex" = (((hackage.alex)."3.2.5").revisions).default;
                "happy" = (((hackage.happy)."1.19.12").revisions).default;
              };
            })
          ];
        })).ghcide.components.exes.ghcide;
  };
}; }