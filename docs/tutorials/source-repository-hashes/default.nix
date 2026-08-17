{ haskell-nix ? (import (builtins.fetchTarball
    "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") {}).pkgs.haskell-nix
} :
let
  pandoc = haskell-nix.hackage-package {
    name         = "pandoc";
    version      = "2.9.2.1";
    # pandoc 2.9.2.1 is from 2020 and needs a GHC of that era.
    compiler-nix-name = "ghc8107";
    index-state  = "2020-04-15T00:00:00Z";
    # Function that returns a sha256 string by looking up the location
    # and tag in a nested attrset
    sha256map =
      { "https://github.com/jgm/pandoc-citeproc"."0.17"
          = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; };
  };
in
  pandoc.components.exes.pandoc
