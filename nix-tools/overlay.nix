final: prev:

let
  compiler-nix-name = "ghc928";

  args = {
    inherit compiler-nix-name;
    compilerSelection = p: p.haskell.compiler;

    # tests need to fetch hackage
    configureArgs = "--disable-tests";

    checkMaterialization = true;
    materialized = ./materialized;
  };

  nix-tools = nix-tools-set args;

  nix-tools-unchecked = nix-tools-set (
    args // { checkMaterialization = false; }
  );

  nix-tools-set = args:
    let
      project = final.haskell-nix.cabalProject'
        [
          {
            evalPackages = final.buildPackages;

            name = "nix-tools";
            src = ./.;

            # Tools to include in the development shell
            shell.tools.cabal = "latest";
          }
          args
        ];

      exes = {
        inherit (project.hsPkgs.nix-tools.components.exes)
          cabal-to-nix
          hashes-to-nix
          plan-to-nix
          hackage-to-nix
          lts-to-nix
          stack-to-nix
          truncate-index
          stack-repos
          cabal-name
          make-install-plan;

        inherit (project.hsPkgs.hpack.components.exes)
          hpack;
      };

      tools = [
        final.buildPackages.nix

        # Why `final.buildPackages.buildPackages.gitMinimal`?
        # Why not just final.buildPackages.gitMinimal?
        #
        # It turns out `git` depends on `gdb` in a round about way:
        #  git -> openssh -> libfido2 -> systemd -> python libxml -> Cython -> gdb
        # Somewhere in that chain there should perhaps be a `buildPackages` so
        # that the `gdb` that is used is not the one for debugging code in
        # the `final` (but instead the one for debugging code in
        # `final.buildPackages`).
        #
        # Using `final.buildPackages.git` causes two problems:
        #
        #   * Multiple versions of `git` (and that dependency chain
        #     to `gdb` are needed when cross compiling).
        #   * When `gdb` does not exist for `js`, so when cross
        #     compiling with ghcjs `final.buildPackages.git` fails
        #     to build at all.
        final.buildPackages.buildPackages.gitMinimal
      ];

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

      toolset = final.buildPackages.symlinkJoin {
        name = "nix-tools";
        paths = builtins.attrValues exes;
        buildInputs = [ final.buildPackages.makeWrapper ];
        meta.platforms = final.lib.platforms.all;
        # We wrap the -to-nix executables with the executables from `tools` (e.g. git)
        # so that consumers of `nix-tools` won't have to provide those tools.
        postBuild = ''
          for prog in stack-to-nix cabal-to-nix plan-to-nix; do
            wrapProgram "$out/bin/$prog" --prefix PATH : "${final.lib.makeBinPath tools}"
          done
        '';
      };
    in
    toolset // { inherit project exes; } // warning;
in
{
  inherit nix-tools nix-tools-unchecked nix-tools-set;
}
