final: prev:
{
  haskell-nix = prev.haskell-nix // {
    # Memoize the cabal-install and nix-tools derivations by adding:
    #   haskell-nix.cabal-install.ghcXXX
    #   haskell-nix.cabal-install-unchecked.ghcXXX
    #   haskell-nix.nix-tools.ghcXXX
    #   haskell-nix.nix-tools-unchecked.ghcXXX
    # Using these avoids unnecessary calls to mkDerivation.
    # For cabal projects we match the versions used to the compiler
    # selected for the project to avoid the chance of a dependency
    # another GHC version (particularly useful on macOS where
    # executables are dynamically linked to GHC itself, which means
    # that if you use a tool built with a different GHC you will get
    # that GHC itself in your closure).
    nix-tools = final.lib.mapAttrs
      (compiler-nix-name: _:
        final.haskell-nix.nix-tools-set { inherit compiler-nix-name; })
      final.haskell-nix.compiler;

    nix-tools-unchecked = final.lib.mapAttrs
      (compiler-nix-name: _:
        final.haskell-nix.nix-tools-set {
          compiler-nix-name =
            # If there is no materialized version for this GHC version fall back on
            # a version of GHC for which there will be.
            if builtins.pathExists (../materialized + "/${compiler-nix-name}/nix-tools/default.nix")
            then compiler-nix-name
            else "ghc928";
          checkMaterialization = false;
        })
      final.haskell-nix.compiler;

    nix-tools-set = { compiler-nix-name, ... }@args:
      let
        compiler-nix-name = "ghc8107";
        compilerSelection = p: p.haskell.compiler;
        project =
          final.haskell-nix.hix.project ({
            evalPackages = final.buildPackages;
            src = ../nix-tools;
          } // args // { inherit compiler-nix-name compilerSelection; });
        exes =
          let
            package = project.getPackage "nix-tools";
          in
          (builtins.map (name: package.getComponent "exe:${name}") [
            "cabal-to-nix"
            "hashes-to-nix"
            "plan-to-nix"
            "hackage-to-nix"
            "lts-to-nix"
            "stack-to-nix"
            "truncate-index"
            "stack-repos"
            "cabal-name"
            "make-install-plan"
          ]) ++ [
            (project.getComponent "hpack:exe:hpack")
          ];
        tools = [
          final.buildPackages.nix
          # Double buildPackages is intentional, see comment in lib/default.nix for details.
          final.buildPackages.buildPackages.gitMinimal
        ];
      in
      (final.buildPackages.symlinkJoin {
        name = "nix-tools";
        paths = exes;
        buildInputs = [ final.buildPackages.makeWrapper ];
        meta.platforms = final.lib.platforms.all;
        # We wrap the -to-nix executables with the executables from `tools` (e.g. git)
        # so that consumers of `nix-tools` won't have to provide those tools.
        postBuild = ''
          for prog in stack-to-nix cabal-to-nix plan-to-nix; do
            wrapProgram "$out/bin/$prog" --prefix PATH : "${final.lib.makeBinPath tools}"
          done
        '';
      }) // {
        inherit project;
        exes = project.hsPkgs.nix-tools.components.exes // {
          hpack = project.hsPkgs.hpack.components.exes.hpack;
        };
      };

    # These `internal` versions are used for:
    # * `nix-tools` for stack projects (since we use `nix-tools` to process
    #   the `stack.yaml` file we cannot match the ghc of the project the
    #   way we do for cabal projects).
    # * Scripts are used to update stackage and hackage
    # Updating the version of GHC selected here should be fairly safe as
    # there should be no difference in the behaviour of these tools.
    # (stack projects on macOS may see a significant change in the
    # closure size of their build dependencies due to dynamic linking).
    internal-cabal-install = final.haskell-nix.cabal-install.ghc8107;
    internal-nix-tools = final.haskell-nix.nix-tools.ghc8107;
  };
}
