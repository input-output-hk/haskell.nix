# This overlay adds hackageQuirks to provide suitable default
# arguments for `haskell-nix.hackage-project` and the functions
# that use it (like `hackage-package`)
#
final: prev:
let
  inherit (final) lib;

in { haskell-nix = prev.haskell-nix // {

  hackageQuirks = { name, version }: {
    # FIXME: this is required to build cabal-install 3.2 with ghc 8.6,
    # but also for
    # https://github.com/input-output-hk/haskell.nix/issues/422
    cabal-install = {
      modules = [ { reinstallableLibGhc = true; } ];
    };

    hlint = {
      modules = [ { reinstallableLibGhc = true; } ];
      pkg-def-extras = [
        (hackage: {
          packages = {
            "alex" = (((hackage.alex)."3.2.5").revisions).default;
          };
        })
      ];
    };

    pandoc = {
      # Function that returns a sha256 string by looking up the location
      # and tag in a nested attrset
      lookupSha256 = { location, tag, ... }:
        { "https://github.com/jgm/pandoc-citeproc"."0.17"
            = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; }
          ."${location}"."${tag}";
      modules = [
        # Windows characters confuse cross compilation
        # See https://github.com/snoyberg/file-embed/pull/33
        (lib.optionalAttrs (version == "2.9.2.1") {
          packages.file-embed.src = final.evalPackages.fetchgit {
            url = "https://github.com/hamishmack/file-embed.git";
            rev = "12b33b8b710517308954c1facff3dc679c2dc5e3";
            sha256 = "0jcpja4s4cylmg9rddyakb1p1fb4l41ffwmy0njpb1dxc5z3v618";
          };
        })
      ];
    };

  }."${name}" or {};

}; }
