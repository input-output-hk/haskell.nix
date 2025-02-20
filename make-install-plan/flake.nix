{
  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs";
    haskellNix.url = "github:input-output-hk/haskell.nix";
  };

  outputs =
    {
      self,
      nixpkgs,
      haskellNix,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-linux"
        "aarch64-darwin"
      ];

      inherit (nixpkgs) lib;
      forAllSystems = f: lib.genAttrs systems (system: f (haskellNix.legacyPackages.${system}));

    in
    {
      project = forAllSystems (pkgs: pkgs.callPackage ./project.nix { });

      packages = forAllSystems (pkgs: rec {
        make-install-plan = pkgs.callPackage ./default.nix { };
        default = make-install-plan;
      });

      checks = lib.mapAttrs (_arch: project: project.flake'.checks) self.project;

      devShells = lib.mapAttrs (_arch: project: { default = project.shell; }) self.project;

      hydraJobs = lib.mapAttrs (_arch: project: project.flake'.hydraJobs) self.project;
    };

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
    allow-import-from-derivation = "true";
  };
}
