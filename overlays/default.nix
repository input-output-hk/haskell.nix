{ sources, ...}@args:

let
  overlays = {
    wine = import ./wine.nix;
    haskell = import ./haskell.nix args;
    nix-tools = (final: prev: {
      haskell-nix = 
        let
          nix-tools-pkgs = import ../nix-tools/overlay.nix final prev;
          nix-tools-unchecked =
            if final.stdenv.hostPlatform.isLinux && final.stdenv.hostPlatform.isx86_64
            then
              let
                tarball = final.fetchzip {
                  name = "nix-tools-0.1.0.0";
                  url = "https://ci.zw3rk.com/build/2912545/download/1/nix-tools-0.1.0.0.tar.gz";
                  sha256 = "sha256-uBQmQLbvbuLTtK5LTgzRjkzYRXj9Zp56qAvheM6V8Mc=";
                };
                nix-tools-provided-exes = builtins.attrValues nix-tools-pkgs.nix-tools.exes;
              in tarball // { exes = final.lib.genAttrs nix-tools-provided-exes (_: tarball); }
            else nix-tools-pkgs.nix-tools;
        in prev.haskell-nix // {
          inherit nix-tools-unchecked;
          inherit (nix-tools-pkgs) nix-tools nix-tools-set;
          # FIXME: is this needed?
          internal-nix-tools = nix-tools-unchecked;
        };
    });
    bootstrap = import ./bootstrap.nix;
    ghc = import ./ghc.nix;
    ghc-packages = import ./ghc-packages.nix;
    hydra = import ./hydra.nix args;
    darwin = import ./darwin.nix;
    windows = import ./windows.nix;
    armv6l-linux = import ./armv6l-linux.nix;
    musl = import ./musl.nix;
    android = import ./android.nix;
    tools = import ./tools.nix;
    emscripten = import ./emscripten.nix;
    nix-prefetch-git-minimal = import ./nix-prefetch-git-minimal.nix;
    gobject-introspection = import ./gobject-introspection.nix;
    hix = import ./hix.nix;
    ghcjs = import ./ghcjs.nix;
    cabalPkgConfig = import ./cabal-pkg-config.nix;
    cacheCompilerDeps = import ./cache-compiler-deps.nix;
    default-setup = import ./default-setup.nix;
    dummy-ghc-data = import ./dummy-ghc-data.nix;
  };

  composeExtensions = f: g: final: prev:
    let
      fApplied = f final prev;
      prev' = prev // fApplied;
    in fApplied // g final prev';

  ordered = with overlays; [
    # Hide nixpkgs haskell and haskellPackages from the haskell-nix overlays.
    # This should prevent us inadvertently depending on them.
    (_: prev: {
      haskell = { };
      haskellPackages = { };
      haskell-nix-prev = prev;
    })
    wine
    haskell
    nix-tools
    bootstrap
    ghc
    ghc-packages
    darwin
    windows
    armv6l-linux
    musl
    android
    tools
    emscripten
    nix-prefetch-git-minimal
    ghcjs
    cabalPkgConfig
    gobject-introspection
    hix
    hydra
    # Restore nixpkgs haskell and haskellPackages
    (_: prev: { inherit (prev.haskell-nix-prev) haskell haskellPackages; })
    dummy-ghc-data
    cacheCompilerDeps
    default-setup
  ];
  combined = builtins.foldl' composeExtensions (_: _: { }) ordered;
in overlays // { inherit combined; }
