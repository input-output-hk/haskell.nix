# These modules are used by `haskell-nix.hackage-project` and the functions
# that use it (like `hackage-package`)
#
let
  # Easier than importing nixpkgs just for this
  mapAttrsToList = f: attrs:
    map (name: f name attrs.${name}) (__attrNames attrs);
in [
  ({config, lib, pkgs, ...}:
    { _file = "haskell.nix/overlays/hackage-quirks.nix#cabal-install"; } //
    lib.mkIf (config.name == "cabal-install") {
      cabalProjectLocal = lib.mkForce (
        # FIXME: this is required to build cabal-install 3.2 with ghc 8.6,
        # but also for
        # https://github.com/input-output-hk/haskell.nix/issues/422
        lib.optionalString (builtins.compareVersions config.version "3.3" < 0) ''
          allow-newer: cabal-install:base, *:base, *:template-haskell
        ''
        # Work around issue with the plan now choosen for older versions
        # of `cabal-install` that causes this error:
        # src/Distribution/Client/FetchUtils.hs:195:36: error:
        #     • Couldn't match type ‘Distribution.Types.PackageId.PackageIdentifier’
        #                      with ‘Cabal-syntax-3.8.1.0:Distribution.Types.PackageId.PackageIdentifier’
        #       NB: ‘Cabal-syntax-3.8.1.0:Distribution.Types.PackageId.PackageIdentifier’
        #             is defined in ‘Distribution.Types.PackageId’
        #                 in package ‘Cabal-syntax-3.8.1.0’
        #           ‘Distribution.Types.PackageId.PackageIdentifier’
        #             is defined in ‘Distribution.Types.PackageId’
        #                 in package ‘Cabal-3.6.3.0’
        # See https://github.com/haskell/cabal/issues/8370
        + lib.optionalString (builtins.compareVersions config.version "3.7" < 0) ''
          constraints: Cabal-syntax <0
        '' + lib.optionalString (__elem config.compiler-nix-name ["ghc961" "ghc96020230302"] && __elem config.version ["3.8.1.0" "3.10.1.0"]) ''
          allow-newer: *:base, *:template-haskell
      '');
      modules = [
        # Version of of cabal-install in hackage is broken for GHC 8.10.1
        (lib.optionalAttrs (config.version == "3.2.0.0"
            && builtins.compareVersions pkgs.buildPackages.haskell-nix.compiler.${config.compiler-nix-name}.version "8.10.0.0" >= 0) {
          packages.cabal-install.src = pkgs.buildPackages.haskell-nix.sources.cabal-32 + "/cabal-install";
        })
      ];
    }
  )

  # The latest version of stack (2.9.1) in hackage fails to build because the
  # of version of rio-prettyprint (recently released 0.1.4.0) chosen by cabal.
  # https://github.com/commercialhaskell/stack/issues/5963
  ({config, lib, pkgs, ...}:
    { _file = "haskell.nix/overlays/hackage-quirks.nix#stack"; } //
    lib.mkIf (config.name == "stack" && builtins.compareVersions config.version "2.9.3" <= 0) {
      cabalProjectLocal = ''
        constraints: unix-compat <0.7${
          lib.optionalString (builtins.compareVersions config.version "2.9.1" <= 0)
            " rio-prettyprint <0.1.4.0"}
      '';
    }
  )

  # Map the following into modules that use `mkIf` to check the name of the
  # hackage package in a way that is lazy enought not to cause infinite recursion
  # issues.
  ] ++ mapAttrsToList (n: v: {config, lib, ...}:
    { _file = "haskell.nix/overlays/hackage-quirks.nix#${n}"; } //
    lib.mkIf (n == config.name) v) {

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
