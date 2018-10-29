{ hackage, pkgs, pkg-def, modules ? [] }: pkgs.lib.evalModules {
  modules = modules ++ [
    ({ lib, ... }: {
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
      };

      # Set the hackage DB for modules/hackage.nix
      hackage.db = hackage;

      # Set the plan for modules/plan.nix
      plan.pkg-def = pkg-def;
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
  ];
}
