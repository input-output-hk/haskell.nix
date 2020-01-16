{
  # allow building for windows
  allowUnsupportedSystem = true;
  # we want the 64bit wine version
  # See overlays/wine.nix as well!
  wine.build = "wine64";
  wine.release = "stable";

  # sadly we need to patch GHC a bit.
  packageOverrides = ps: with ps; let
  in rec {
     rocksdb = with ps.stdenv;
     if hostPlatform.isWindows
     then pkgs.callPackage ./pkgs/rocksdb-prebuilt.nix { inherit (buildPackages) fetchurl unzip; }
     else ps.rocksdb;
  };
}

