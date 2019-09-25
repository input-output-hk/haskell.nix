{ nixpkgs ? <nixpkgs>
, nix-tools-path
, index-state
, hash
}:

rec {
  pkgs = import nixpkgs {};
  hsPkgs = import nix-tools-path { inherit nixpkgs; };
  index = builtins.fetchurl http://hackage.haskell.org/01-index.tar.gz;
  indexTruncated = pkgs.runCommand "00-index.tar.gz" {
    outputHashAlgo = "sha256";
    outputHash = hash;
  } ''
    ${hsPkgs.nix-tools.components.exes.truncate-index}/bin/truncate-index -o $out -i ${index} -s ${index-state}
  '';
}
