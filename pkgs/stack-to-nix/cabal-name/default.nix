{ pkgs ? import <nixpkgs> {} }: with pkgs;

let
  project = { mkDerivation, base, Cabal }: mkDerivation {
    pname = "cabal-name";
    version = "1";

    src = lib.cleanSource ./.;

    isExecutable = true;
    isLibrary = false;

    executableHaskellDepends = [ base Cabal ];

    license = lib.licenses.free;
  };
in

haskellPackages.callPackage project {}
