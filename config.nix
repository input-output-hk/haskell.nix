{
  # allow building for windows
  allowUnsupportedSystem = true;
  # we want the 64bit wine version
  wine.build = "wine64";
  # once https://github.com/NixOS/nixpkgs/pull/71216 is
  # merged, we can use `series3`; wine4 seems to suffer
  # from some non-obvious regression :(
  # This will also need to be backported to 19.03 and 19.09
  # at least...
  wine.release = "series3";

  # sadly we need to patch GHC a bit.
  packageOverrides = ps: with ps; let
  in rec {
     rocksdb = with ps.stdenv;
     if hostPlatform.isWindows
     then pkgs.callPackage ./pkgs/rocksdb-prebuilt.nix { inherit (buildPackages) fetchurl unzip; }
     else ps.rocksdb;
  };
}

