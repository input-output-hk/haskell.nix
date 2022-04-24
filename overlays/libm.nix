self: super:

{
  # Some packages have cabal deps on `m`, which just comes with the C compiler.
  # So we create a dummy package to satisfy haskell.nix's dependency resolution.
  m = self.stdenv.mkDerivation {
    name = "m";
    unpackPhase = "true";
    # We have to create lib and bin to make it "look like" a library
    installPhase = ''
    mkdir -p $out/lib
    mkdir -p $out/bin
    '';
  };
}
