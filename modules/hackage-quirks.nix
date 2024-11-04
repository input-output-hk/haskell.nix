# These modules are used by `haskell-nix.hackage-project` and the functions
# that use it (like `hackage-package`)
#
let
  # Easier than importing nixpkgs just for this
  mapAttrsToList = f: attrs:
    map (name: f name attrs.${name}) (__attrNames attrs);
in [
  # Avoid pantry 0.9 in versions without https://github.com/commercialhaskell/stack/pull/6187
  # Also avoid optparse-applicative 0.18
  # http-download 0.2.1 https://github.com/commercialhaskell/stack/issues/6210
  ({config, lib, pkgs, ...}:
    { _file = "haskell.nix/overlays/hackage-quirks.nix#stack"; } //
    lib.mkIf (config.name == "stack") {
      cabalProjectLocal =
        if builtins.compareVersions config.version "2.11.1" <= 0 then ''
          constraints: pantry <0.9, optparse-applicative <0.18, http-download <0.2.1
        ''
        else if builtins.compareVersions config.version "2.11.1.1" <= 0 then ''
          constraints: http-download <0.2.1
        ''
        else "";
    }
  )

  # Map the following into modules that use `mkIf` to check the name of the
  # hackage package in a way that is lazy enought not to cause infinite recursion
  # issues.
  ] ++ mapAttrsToList (n: v: {config, lib, ...}:
    { _file = "haskell.nix/overlays/hackage-quirks.nix#${n}"; } //
    lib.mkIf (n == config.name) v) {

    # See https://github.com/input-output-hk/haskell.nix/issues/2277
    hoogle = {
      cabalProject = ''
        packages: .
        allow-newer: hoogle:crypton-connection
      '';
    };

    lsp-test = {
      cabalProject = ''
        packages: .
        package lsp
          flags: +demo
      '';
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
       {pkgs, lib, ...}: lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
         # The order of -lssl and -lcrypto is important here
         packages.postgrest.configureFlags = [
           "--ghc-option=-optl=-lssl"
           "--ghc-option=-optl=-lcrypto"
           "--ghc-option=-optl=-L${pkgs.openssl.out}/lib"
         ];
      })];
    };

    stack = {
      modules = [{
        # Stack has a custom setup that expects both the library and stack executable
        # to be configured at the same time.  Unfortunately this does mean that
        # the library component is rebuilt unecessarily in the exe component derivation.
        packages.stack.components.exes.stack.configureAllComponents = true;
        # But we don't want to configure the tests as they have dependencies that
        # are not included in the `exes` dependencies.
        packages.stack.components.exes.stack.configureFlags = ["--disable-tests"];
      }];
    };
  }
