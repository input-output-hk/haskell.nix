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
        allow-newer: cabal-install:base
      '';
      modules = [
        { reinstallableLibGhc = true; }
        # Version of of cabal-install in hackage is broken for GHC 8.10.1
        (lib.optionalAttrs (version == "3.2.0.0") {
          packages.cabal-install.src = final.haskell-nix.sources.cabal-32 + "/cabal-install";
          # Include patches needed for ghcjs
          packages.Cabal.patches = [
            ./patches/Cabal/Cabal-3.0.0.0-drop-pkg-db-check.diff
            ./patches/Cabal/Cabal-3.0.0.0-no-final-checks.diff
          ];
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

    haskell-language-server = {
      cabalProject = ''
        packages: .
        allow-newer: haskell-language-server:ghcide
        constraints: ghcide <0.7.4, hls-plugin-api <0.7.1.0, hls-retrie-plugin <0.1.1.1
      '';
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
