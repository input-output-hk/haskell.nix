final: prev:

let
  mk-nix-tools = { checkMaterialization ? true }:
    let
      toolset = final.haskell-nix.nix-tools-set {
        compiler-nix-name = "ghc8107";
        inherit checkMaterialization;
      };
      warning = final.lib.mapAttrs
        (_: _:
          final.lib.warn
            ''
              The package nix-tools is now compiled with a single GHC version.
              You can use the function nix-tools-set to compile nix-tools using a specific compiler: 

                nix-tools-set { compiler-nix-name = " "ghcXYZ" "; }

            ''
            toolset
        )
        final.haskell-nix.compiler;
    in
    toolset // warning;

in
{
  haskell-nix = prev.haskell-nix // {
    nix-tools = mk-nix-tools { };
    nix-tools-unchecked = mk-nix-tools { checkMaterialization = false; };

    nix-tools-set = args:
      let
        project =
          final.haskell-nix.hix.project ({
            evalPackages = final.buildPackages;
            src = ../nix-tools;
          } // args // {
            compilerSelection = p: p.haskell.compiler;
          });
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
