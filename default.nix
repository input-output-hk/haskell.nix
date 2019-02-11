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
                                             rev    = "ed1dbc01f98411894f6613c62818f14b02fb6679";
                                             sha256 = "0kbj4kb9rlvjb4afpcisz9zlk5z3h7frkwggfwri1q5683fapkgv";
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
