let
  overrideWith = override: default:
   let
     try = builtins.tryEval (builtins.findFile builtins.nixPath override);
   in if try.success then
     builtins.trace "using search host <${override}>" try.value
   else
     default;
in

{ pkgs ? import <nixpkgs> {}

# a different haskell infrastructure
, haskell ? import (overrideWith "haskell"
                     (pkgs.fetchFromGitHub { owner  = "input-output-hk";
                                             repo   = "haskell.nix";
                                             rev    = "7446d044a0db6acdd3d13c1d933ec1ed6da9a606";
                                             sha256 = "11zsf22r98xxr0wj38mm9ig9gsgk5c3wf0xcsxfbf7xxyg4v22iv";
                                             name   = "haskell-lib-source"; }))
                   { inherit pkgs; }
}:

let
  hsPkgs = import ./pkgs.nix { inherit (haskell) mkPkgSet; };
in
  hsPkgs // {
    nix-tools-all-execs = pkgs.symlinkJoin
      { name = "nix-tools";
        paths = builtins.attrValues hsPkgs.nix-tools.components.exes; };
  }
