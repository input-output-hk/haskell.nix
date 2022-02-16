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
      cabalProject = ''
        packages: .
        allow-newer: cabal-install:base, *:base, *:template-haskell
      '';
      modules = [
        { reinstallableLibGhc = true; }
        # Version of of cabal-install in hackage is broken for GHC 8.10.1
        (lib.optionalAttrs (version == "3.2.0.0") {
          packages.cabal-install.src = final.haskell-nix.sources.cabal-32 + "/cabal-install";
        })
      ];
    };

    hpack = {
      modules = [ { reinstallableLibGhc = true; } ];
    };

    hlint = {
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
      sha256map =
        { "https://github.com/jgm/pandoc-citeproc"."0.17"
            = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; };
    };

    # See https://github.com/input-output-hk/haskell.nix/issues/948
    postgrest = {
      cabalProject = ''
        packages: .
        package postgresql-libpq
          flags: +use-pkg-config
      '';
      modules = [(
       {pkgs, ...}: final.lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
         # The order of -lssl and -lcrypto is important here
         packages.postgrest.configureFlags = [
           "--ghc-option=-optl=-lssl"
           "--ghc-option=-optl=-lcrypto"
           "--ghc-option=-optl=-L${pkgs.openssl.out}/lib"
         ];
      })];
    };

  }."${name}" or {};

}; }
