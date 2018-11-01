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
                                           sha256 = "12ffzzjgirwzha3ngxbniccgn19406iryxspq19kgi4kz9lz6bpr"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "haskell.nix";
                                            rev    = "e68f852d6e9f190785dd1f26c422fe9b2cfc5750";
                                            sha256 = "1kby8qzag4qsbx6agfb08lbnwqd4zdfjq729c5gk6jyrrmpwwn7p"; }))
                   hackage;

  # our packages
  plan = import ./plan.nix;

  pkgSet = haskell.mkNewPkgSet {
    inherit pkgs;
    pkg-def = plan;
    modules = [{
      packages = {
        nix-tools = import ./nix-tools.nix;
        hackage-db = import ./hackage-db.nix;
      };
    }];
  };

  packages = pkgSet.config.hsPkgs;

in packages
