{ mkPkgSet }:

let

  # our packages
  # this is generated with plan-to-nix
  # $ cabal new-configure
  # $ plan-to-nix ./dist-newstyle/cache/plan.json > nix/.plan.nix
  plan = import ./nix/.plan-pkgs.nix;

  pkgSet = mkPkgSet {
    pkg-def = plan.pkgs;
    pkg-def-overlays = [
      plan.overlay
    ];
    modules = [
      # specific package overrides would go here
      ({ lib, ... }: {
        packages.nix-tools.src = lib.cleanSource ./.;
      })
    ];
  };

  packages = pkgSet.config.hsPkgs;

in packages
