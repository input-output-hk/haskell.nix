# Writes the store paths of a list of sources into a file.
# If this derivation is built by Hydra, then the sources will
# be kept and will be available to download from the binary cache.
{ lib, writeTextFile
, sources # A list of paths
}:

writeTextFile {
  name = "haskell.nix-source-pins";
  text = lib.concatMapStringsSep "\n" toString sources;
}
