{ mkPkgSet }:

let

  # our packages
  # this is generated with plan-to-nix
  # $ cabal new-configure
  # $ plan-to-nix ./dist-newstyle/cache/plan.json > plan.nix
  plan = import ./plan.nix;

  pkgSet = mkPkgSet {
    pkg-def = plan;
    pkg-def-overlays = [
      { nix-tools  = ./nix-tools.nix;
        # these are generated with cabal-to-nix
        # $ cabal-to-nix https://github.com/galenhuntington/haskell-src-meta.git 109ee29d5fd0f4e23fdd2f80eb122d66341b64a9 > haskell-src-meta.nix
        hackage-db       = ./hackage-db.nix;
        haskell-src-meta = ./haskell-src-meta.nix;
        hnix             = ./hnix.nix;
      }
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
