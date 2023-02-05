{ with-nixpkgs ? true }:
let

  haskell-nix = import ../../default.nix { };
  nixpkgs = haskell-nix.inputs.nixpkgs.outPath;

  o2 = (
    final: prev: {
      haskell-nix = prev.haskell-nix // {
        compiler = prev.haskell-nix.compiler // {
          ghc8107 = prev.haskell.compiler.ghc8107 // {
            # Add stuff not in nixpkgs ghc
            configured-src = prev.haskell-nix.compiler.ghc8107.configured-src; # Needed for reinstallableLibGhc to work
          };
        };
      };
    }
  );

  pkgs = import nixpkgs {
    inherit (haskell-nix) config;
    overlays = [
      haskell-nix.overlay
    ]
    ++
    (if with-nixpkgs then [ o2 ] else [ ]);
  };

  prj = pkgs.haskell-nix.cabalProject {
    src = ./empty;
    cabalProject = "extra-packages: lens";
    compiler-nix-name = "ghc8107";
  };
in

prj.plan-nix
