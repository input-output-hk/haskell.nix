{ haskell-nix, testSrc } :
let
  pandoc = haskell-nix.hackage-package {
    name         = "pandoc";
    version      = "2.9.2.1";
    index-state  = "2020-04-15T00:00:00Z"; 
    # Function that returns a sha256 string by looking up the location
    # and tag in a nested attrset
    sha256map =
      { "https://github.com/jgm/pandoc-citeproc"."0.17"
          = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; };
  };
in
  pandoc.components.exes.pandoc