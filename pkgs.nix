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
                                            rev    = "20ad44ecdd5475c8adbe0e129638f729a26ca120";
                                            sha256 = "0bh0p58i9w9nw2mcjgx6j9qyi6x5xg8pn5x37a696kw1bgwm8wzn"; }))
                   ;
  # a different haskell infrastructure
  haskell = import (overrideWith "haskell"
                    (pkgs.fetchFromGitHub { owner  = "angerman";
                                            repo   = "haskell.nix";
                                            rev    = "fe5c13d8b72cf5335e5b6f71ffa3828def716736";
                                            sha256 = "183i77jn2bg1669hpjikm28hgznwdzndjpgwvm3mdpx4kkp4ambk"; }))
                   hackage;

  # our packages
  plan = import ./plan.nix;

  pkgSet = haskell.mkPkgSet pkgs plan;

  packages = pkgSet {
    extraDeps = hsPkgs: { nix-tools = ./nix-tools.nix; }; };
in packages
