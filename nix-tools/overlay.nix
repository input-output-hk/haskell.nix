final: _prev:

let
  compiler-nix-name = "ghc96";

  nix-tools = nix-tools-set {
    nix-tools = nix-tools-unchecked;
  };

  nix-tools-unchecked = nix-tools-set {};

  nix-tools-eval-on-linux = nix-tools-set {
    evalSystem = builtins.currentSystem or "x86_64-linux";
  };

  nix-tools-set = args:
    let
      project = final.haskell-nix.cabalProject'
        [
          {
            name = "nix-tools";
            src = ./.;

            compiler-nix-name = final.lib.mkDefault compiler-nix-name;
            # compilerSelection = p: p.haskell.compiler;

            # tests need to fetch hackage
            configureArgs = final.lib.mkDefault "--disable-tests";

            # Tools to include in the development shell
            shell.tools.cabal = {};
            shell.tools.haskell-language-server = {};
          }
          args
        ];

      # pick the version from the nix-tools cabal package, not that it really matters ...
      name = "nix-tools-${project.hsPkgs.nix-tools.identifier.version}";

      exes = {
        inherit (project.hsPkgs.cabal-install.components.exes)
          cabal;

        inherit (project.hsPkgs.nix-tools.components.exes)
          cabal-name
          cabal-to-nix
          default-setup
          default-setup-ghcjs
          hackage-to-nix
          hashes-to-nix
          lts-to-nix
          make-install-plan
          plan-to-nix
          stack-repos
          stack-to-nix
          truncate-index;

        inherit (project.hsPkgs.hpack.components.exes)
          hpack;

        inherit (project.hsPkgs.Cabal-syntax-json.components.exes)
          cabal2json;
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

      toolset = final.buildPackages.symlinkJoin {
        inherit name;
        paths = builtins.attrValues exes;
        buildInputs = [ final.buildPackages.makeWrapper ];
        meta.platforms = final.lib.platforms.all;
        passthru = { inherit project exes; };
      };
    in
    toolset // warning;
in
{
  inherit nix-tools nix-tools-unchecked nix-tools-eval-on-linux nix-tools-set;
}
