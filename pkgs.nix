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
                                           rev    = "4815049bf755492e63ef18129b7f3b0fa62eb155";
                                           sha256 = "1fjyyfdbn0657qqj8h0bagjsjdgf0s2da30nzjwk3zkfb5k89l31"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "haskell.nix";
                                            rev    = "13898161f0353cfacc84fd038d74596b62dad662";
                                            sha256 = "161wbhyy0lqlrpdwixsjbq6wxybs75wwil8gi52vhccwhqqjk0k8"; }))
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
