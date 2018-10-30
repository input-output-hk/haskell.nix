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
                                           rev    = "5910ad8039568239082e71eae18117fdcac94d89";
                                           sha256 = "12ffzzjgirwzha3ngxbniccgn19406iryxspq19kgi4kz9lz6bpr"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "haskell.nix";
                                            rev    = "866042b9bfe7cd44ff794efee5eb0fae762c5fbe";
                                            sha256 = "05vzgx3frr7jrzhkpdrrknd2c29dynsxsdaalh39hzq95j175pmm"; }))
                   hackage;

  # our packages
  plan = import ./plan.nix;

  pkgSet = haskell.mkNewPkgSet pkgs plan;

  packages = pkgSet {
    extraDeps = {
      nix-tools = ./nix-tools.nix;
      hackage-db = ./hackage-db.nix;
    }; };
in packages
