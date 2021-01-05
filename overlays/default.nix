args:

let
  overlays = {
    wine = import ./wine.nix;
    #ghcjs = import ./ghcjs-asterius-triple.nix;
    #python = import ./python.nix;
    haskell = import ./haskell.nix args;
    default-modules = import ./default-modules.nix;
    hackage-quirks = import ./hackage-quirks.nix;
    bootstrap = import ./bootstrap.nix;
    ghc = import ./ghc.nix;
    ghc-packages = import ./ghc-packages.nix;
    darwin = import ./darwin.nix;
    windows = import ./windows.nix;
    armv6l-linux = import ./armv6l-linux.nix;
    musl = import ./musl.nix;
    tools = import ./tools.nix;
    emscripten = import ./emscripten.nix;
    nix-prefetch-git-minimal = import ./nix-prefetch-git-minimal.nix;
    gobject-introspection = import ./gobject-introspection.nix;
    eval-on-current = import ./eval-on-current.nix;
    eval-on-build = import ./eval-on-build.nix;
    ghcjs = import ./ghcjs.nix;
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
    hackage-quirks
    default-modules
    bootstrap
    ghc
    ghc-packages
    darwin
    windows
    armv6l-linux
    musl
    tools
    emscripten
    nix-prefetch-git-minimal
    ghcjs
    gobject-introspection
    # Restore nixpkgs haskell and haskellPackages
    (_: prev: { inherit (prev.haskell-nix-prev) haskell haskellPackages; })
  ];
  combined = builtins.foldl' composeExtensions (_: _: { })
    (ordered ++ [overlays.eval-on-current]);
  combined-eval-on-build = builtins.foldl' composeExtensions (_: _: { })
    (ordered ++ [overlays.eval-on-build]);
in overlays // { inherit combined combined-eval-on-build; }
