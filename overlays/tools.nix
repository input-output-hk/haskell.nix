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
    ghcide.object-code = args:
        (final.haskell-nix.cabalProject (args // {
          name = "ghcide";
          src = final.fetchFromGitHub {
            owner = "mpickering";
            repo = "ghcide";
            rev = "706c59c97c25c66798815c1dc3ee6885a298918a";
            sha256 = "0d158xifwvz0y69ah98ckxakzqpz229mq7rpf2bpbmwhnpw3jmm6";
          };
          modules = [({config, ...}: {
            packages.ghcide.configureFlags = lib.optional (!final.stdenv.targetPlatform.isMusl)
                                              "--enable-executable-dynamic";
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

    haskell-language-server = {
      "0.5.1" = args:
        (final.haskell-nix.cabalProject ( args // {
          name = "haskell-language-server";
          src = final.fetchFromGitHub {
            owner = "haskell";
            repo = "haskell-language-server";
            rev = "0.5.1";
            sha256 = "17nzgpiacmrvwsy2fjx6a6pcpkncqcwfhaijvajm16jpdgni8mik";
            fetchSubmodules = true;
          };
          sha256map = {
            "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
          };
          # Plan issues with the benchmarks, can try removing later
          configureArgs = "--disable-benchmarks";
          modules = [{
            # Tests don't pass for some reason, but this is a somewhat random revision.
            packages.haskell-language-server.doCheck = false;
          }];
        })).haskell-language-server.components.exes.haskell-language-server;
      # When adding new versions here, please set "latest" too the latest version
      # so that `tools = { haskell-language-server = "latest"; }`
      # will work the same way it does for tools that are in hackage.
      "latest" = final.haskell-nix.custom-tools.haskell-language-server."0.9.0";
    } // final.lib.mapAttrs (rev: sha256:
      args:
        (final.haskell-nix.cabalProject ( args // {
          name = "haskell-language-server";
          src = final.fetchFromGitHub {
            owner = "haskell";
            repo = "haskell-language-server";
            inherit rev sha256;
            fetchSubmodules = true;
          };
          sha256map = {
            "https://github.com/bubba/brittany.git"."c59655f10d5ad295c2481537fc8abf0a297d9d1c" = "1rkk09f8750qykrmkqfqbh44dbx1p8aq1caznxxlw8zqfvx39cxl";
            "https://github.com/bubba/hie-bios.git"."cec139a1c3da1632d9a59271acc70156413017e7" = "1iqk55jga4naghmh8zak9q7ssxawk820vw8932dhympb767dfkha";
            "https://github.com/alanz/ghc-exactprint.git"."6748e24da18a6cea985d20cc3e1e7920cb743795" = "18r41290xnlizgdwkvz16s7v8k2znc7h215sb1snw6ga8lbv60rb";
          };
          # Plan issues with the benchmarks, can try removing later
          configureArgs = "--disable-benchmarks";
        })).haskell-language-server.components.exes.haskell-language-server
    ) {
      "0.6.0" = "027fq6752024wzzq9izsilm5lkq9gmpxf82rixbimbijw0yk4pwj";
      "0.7.1" = "0gkzvjx4dgf53yicinqjshlj80gznx5khb62i7g3kqjr85iy0raa";
      "0.8.0" = "0p6fqs07lajbi2g1wf4w3j5lvwknnk58n12vlg48cs4iz25gp588";
      "0.9.0" = "18g0d7zac9xwywmp57dcrjnvms70f2mawviswskix78cv0iv4sk5";
    };
  };
}; }
