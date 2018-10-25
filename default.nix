{ pkgs ? import <nixpkgs> {} }:
let
 base = import ./pkgs.nix { inherit pkgs; };
 overlays = [
   ((import ./plan.nix null).overlay pkgs)
   (self: super: with pkgs.haskell.lib; {
     hnix = dontCheck super.hnix;
     time = dontCheck super.time;
     bytestring = dontCheck super.bytestring;
     directory = dontCheck super.directory;
     containers = dontCheck super.containers;
     text = dontCheck super.text;
     aeson = dontCheck super.aeson;
     binary = dontCheck super.binary;
     http-client = dontCheck super.http-client;
     network-uri = dontCheck super.network-uri;
     parsec = dontCheck super.parsec;
     Cabal = dontCheck super.Cabal;
   })
 ];
in
 base#builtins.foldl' (pkgs: overlay: pkgs.extend overlay) base overlays
