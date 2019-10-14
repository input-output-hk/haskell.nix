{
  # allow building for windows
  allowUnsupportedSystem = true;
  # we want the 64bit wine version
  wine.build = "wine64";
  # once https://github.com/NixOS/nixpkgs/pull/68397 is
  # merged, we can use `series3`; wine4 seems to suffer
  # from some non-obvious regression :(
  wine.release = "stable"; #"series3";

  # sadly we need to patch GHC a bit.
  packageOverrides = ps: with ps; let
  in rec {
     rocksdb = with ps.stdenv;
     if hostPlatform.isWindows
     then pkgs.callPackage ./pkgs/rocksdb-prebuilt.nix { inherit (buildPackages) fetchurl unzip; }
     else ps.rocksdb;
  };
}

