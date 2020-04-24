# A loosely curated set of tools we can select with:
#   haskell-nix.tools.cabal
# or
#   shellFor { tools = ts: with ts; [ cabal ]; }
#
# It will be compiled with the same version of ghc used in the
# shell (the build ghc in the case of cross compilation).
#
# If want to specify the ghc used to compile. Use:
#   haskell-nix.tools.ghc883.cabal
# or
#   shellFor { tools = ts: with ts; [ ghc883.cabal ]; }
#
# By default we will get the latest version of the tool.
# To specify the version use:
#   haskell-nix.tools.cabal."3.2.0.0"
# or
#   shellFor { tools = ts: with ts; [ cabal."3.2.0.0" ]; }
#
# To add a new tool to this file:
#   * Add it to `hackageToolVersions`.
#   * Test it with:
#       nix-build -E 'let h = (import ./. {}); in (import h.sources.nixpkgs-default h.nixpkgsArgs).haskell-nix.tools.ghc865.cabal."3.2.0.0"'
#   * If necessary add `modules` or other args it needs to `hackageToolArgs`.
#
# To add a new tool that is not in hackage:
#   * Add it to `otherTools` in `toolsForGhc`.
#
# To include materialization for a tool:
#   * Build the tool with `checked-tools` instead of `tools`
#     (it will fail to build since the materialized
#     files are missing):
#       nix-build -E 'let h = (import ./. {}); in (import h.sources.nixpkgs-default h.nixpkgsArgs).haskell-nix.checked-tools.ghc865.cabal."3.2.0.0"'
#   * Look in the logs for something like:
#       Materialized nix used for cabal-install-plan-to-nix-pkgs is missing. To fix run :
#         cp -r /nix/store/dwhg7djxwzd0bg69m3fqhywk7fbyf01i-cabal-install-plan-to-nix-pkgs /Users/hamish/iohk/haskell.nix/materialized/tools/ghc-8.6.5-cabal-3.2.0.0
#         chmod -R +w /Users/hamish/iohk/haskell.nix/materialized/tools/ghc-8.6.5-cabal-3.2.0.0
#   * Run the two commands from the log
#   * Run `git add materialized` before committing the changes.
self: super:
let
  inherit (self) lib;

  # Tools and versions to include from hackage
  hackageToolVersions = {
    # Default is version the head of the list
    cabal           = ["3.2.0.0" "3.0.0.0" "2.4.1.0"];
    stylish-haskell = ["0.11.0.0" "0.10.0.0"];
    hlint           = ["2.2.11"];
  };

  # Some times the package name in hackage is not the same as tool name.
  packageName = {
    cabal = "cabal-install";
  };

  # Some hackage packages require some `modules` to work.
  hackageToolArgs = {
    # FIXME: this is required to build cabal-install 3.2 with ghc 8.6,
    # but also as for
    # https://github.com/input-output-hk/haskell.nix/issues/422
    cabal = {
      modules = [ { reinstallableLibGhc = true; } ];
    };
    hlint = {
      modules = [ { reinstallableLibGhc = true; } ];
      pkg-def-extras = [
        (hackage: {
          packages = {
            "alex" = (((hackage.alex)."3.2.5").revisions).default;
          };
        })
      ];
    };
  };

  toolsForGhc =
      { materializedOnly ? false
      , index-state ? self.haskell-nix.internalHackageIndexState
      , checkMaterialization ? self.haskell-nix.checkMaterialization
      }: ghc:
    let
      materializationPath = packageName: packageVersion:
        ../materialized/tools
          + "/ghc-${ghc.version}-${packageName}-${packageVersion}";
      materializationPathExists = packageName: packageVersion:
        builtins.pathExists (materializationPath packageName packageVersion);
      materializationAttrs = packageName: packageVersion: {
          inherit ghc index-state checkMaterialization;
          # Include the materialized path if it exists or to improve error
          # messages when we are checking materialization.
          materialized =
            if checkMaterialization
                || materializationPathExists packageName packageVersion
              then materializationPath packageName packageVersion
              else null;
        };
      
      # Tools that we can get from hackage
      hackageTools =
        lib.mapAttrs (name: versions:
          let
            defaultVersion = lib.head versions;
            toolForVersion = version:
              (self.haskell-nix.hackage-package ({
                name = packageName."${name}" or name;
                inherit version;
              } // (hackageToolArgs."${name}" or {})
                // materializationAttrs name version)).components.exes."${name}";
          in lib.optionalAttrs (!materializedOnly) (toolForVersion defaultVersion) //
            lib.listToAttrs (
              builtins.map
                (version: lib.nameValuePair version (toolForVersion version))
                (lib.filter (v: !materializedOnly || materializationPathExists name v) versions))
        ) hackageToolVersions;

      # Tools not in hackage yet
      otherTools = {
        inherit ((self.haskell-nix.cabalProject ({
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
        } // materializationAttrs "ghcide" "github")).ghcide.components.exes) ghcide;
      };
  in hackageTools // otherTools;
in {
  haskell-nix = super.haskell-nix // {

    inherit toolsForGhc;
    
    tools =
      toolsForGhc {} self.haskell-nix.ghc //
      lib.mapAttrs (_: toolsForGhc {}) self.haskell-nix.compiler;

    # Like `tools` but allows you to quickly check the materialization
    # of the tool is correct.
    checked-tools =
      toolsForGhc { checkMaterialization = true; } self.haskell-nix.ghc //
      lib.mapAttrs (_: toolsForGhc { checkMaterialization = true; }) self.haskell-nix.compiler;
  };
}