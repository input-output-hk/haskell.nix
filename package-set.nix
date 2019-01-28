let f = { hackage, pkgs, pkg-def, pkg-def-overlays ? [], modules ? [] }: let
  buildModules = f { inherit hackage pkg-def pkg-def-overlays modules; pkgs = pkgs.buildPackages; };
in pkgs.lib.evalModules {
  modules = modules ++ [
    ({ config, lib, ... }: {
      # Provide all modules with haskellLib, pkgs, and pkgconfPkgs arguments
      _module.args = {
        # this is *not* the hasekllLib from nixpkgs; it is rather our own
        # library from haskell.nix
        haskellLib = let hl = import ./lib { inherit lib; haskellLib = hl; }; in hl;

        # The package descriptions depend on pkgs, which are used to resolve system package dependencies
        # as well as pkgconfPkgs, which are used to resolve pkgconfig name to nixpkgs names.  We simply
        # augment the existing pkgs set with the specific mappings:
        pkgs = pkgs // (import ./lib/system-nixpkgs-map.nix pkgs);
        pkgconfPkgs = pkgs // (import ./lib/pkgconf-nixpkgs-map.nix pkgs);

        inherit buildModules;
      };

      # Set the hackage DB for modules/hackage.nix
      hackage.db = hackage;

      # Set the plan for modules/plan.nix
      plan.pkg-def = with builtins;
        # The desugar reason.
        #
        # it is quite combersome to write
        # (hackage: { packages.x.revision = hackage...;
        #             packages.y.revision = import ./foo.nix; })
        # where we'd rather write:
        # (hackage: { x = hackage...; })
        # or
        # { y = ./foo.nix; }
        # As such the desugarer desugars this short hand syntax.
        let
          isPath  = x: builtins.typeOf x == "path";
          # rewrite
          #   { ... }
          # into
          #   { package = { ... }; }
          inject-packages = o: if o ? "packages" then o else { packages = o; };
          # rewrite
          #   x = pkg;
          # into
          #   x.revision = pkg;
          inject-revision = pkg: if pkg ? "revision" then pkg else { revision = pkg; };
          # rewrite
          #   x.revision = ./some/path;
          # into
          #   x.revision = import ./some/path;
          expand-paths = pkg: if !(isPath pkg.revision) then pkg else { revision = import pkg.revision; };
          # apply injection and expansion to the "packages" in overlay.
          desugar = overlay: lib.mapAttrs (k: v: if k != "packages"
                              then v
                              else lib.mapAttrs (_: pkg: (expand-paths (inject-revision pkg))) v)
                                                (inject-packages overlay);
        # fold any potential `pkg-def-overlays`
        # onto the `pkg-def`.
        #
        # This means you can have a base definition (e.g. stackage)
        # and augment it with custom packages to your liking.
        in foldl' lib.recursiveUpdate
            (pkg-def config.hackage.configs)
            (map (p: desugar (if builtins.isFunction p then p config.hackage.configs else p)) pkg-def-overlays)
      ;

    })

    # Supplies metadata
    ./modules/cabal.nix

    # Converts config.packages into config.hsPkgs
    # Replace this with compat-driver.nix to use nixpkgs haskell build infra
    ./modules/component-driver.nix

    # Converts config.hackage.db to config.hackage.configs
    ./modules/hackage.nix

    # Converts config.hackage.configs and pkg-def to config.packages
    ./modules/plan.nix

    # Configuration that applies to all plans
    ./modules/configuration-nix.nix
  ];
};
in f
