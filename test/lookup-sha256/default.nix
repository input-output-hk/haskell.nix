{ pkgs, lib, stdenv, haskell-nix, testSrc, zlib } :
let
  pandoc = haskell-nix.hackage-package {
    name         = "pandoc";
    version      = "2.9.2.1";
    index-state  = "2020-04-15T00:00:00Z"; 
    # Function that returns a sha256 string by looking up the location
    # and tag in a nested attrset
    lookupSha256 = { location, tag, ... }:
      { "https://github.com/jgm/pandoc-citeproc"."0.17"
          = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; }
        ."${location}"."${tag}";
    modules = [
      # Windows characters confuse cross compilation
      # See https://github.com/snoyberg/file-embed/pull/33
      {
        packages.file-embed.src = pkgs.fetchgit {
          url = "https://github.com/hamishmack/file-embed.git";
          rev = "12b33b8b710517308954c1facff3dc679c2dc5e3";
          sha256 = "0jcpja4s4cylmg9rddyakb1p1fb4l41ffwmy0njpb1dxc5z3v618";
        };
      }
      # Musl needs static zlib
      (lib.optionalAttrs stdenv.hostPlatform.isMusl {
        packages.pandoc.components.exes.pandoc.configureFlags = [
          "--ghc-option=-optl=-L${zlib.static}/lib"
        ];
      })
    ];
  };
in
  pandoc.components.exes.pandoc

