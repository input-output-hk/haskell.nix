{ pkgs ? import <nixpkgs> {}
}:
let
  overrideWith = override: default:
   let
     try = builtins.tryEval (builtins.findFile builtins.nixPath override);
   in if try.success then
     builtins.trace "using search host <${override}>" try.value
   else
     default;
in
let
  # all packages from hackage as nix expressions
  hackage = import (overrideWith "hackage"
                   (pkgs.fetchFromGitHub { owner  = "angerman";
                                           repo   = "hackage.nix";
                                           rev    = "66c28064da46525711722b75b4adb2ac878897d3";
                                           sha256 = "12ffzzjgirwzha3ngxbniccgn19406iryxspq19kgi4kz9lz6bpr";
                                           name   = "hackage-exprs-source"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "haskell.nix";
                                            rev    = "2a3b2612a15fd7f14d32c3519aba2b64bd7b1e43";
                                            sha256 = "181dv1zlf381kkb82snjmpibhgmkyw1n5qsvpqjrv8dxmcjqjl2k";
                                            name   = "haskell-lib-source"; }))
                   hackage;

  # our packages
  plan = import ./plan.nix;

  pkgSet = haskell.mkNewPkgSet {
    inherit pkgs;
    pkg-def = plan;
    pkg-def-overlays = [
      { nix-tools = ./nix-tools.nix;
        hackage-db = ./hackage-db.nix;
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
