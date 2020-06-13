args:

let
  overlays = {
    wine = import ./wine.nix;
    #ghcjs = import ./ghcjs-asterius-triple.nix;
    #python = import ./python.nix;
    haskell = import ./haskell.nix args;
    hackage-quirks = import ./hackage-quirks.nix;
    bootstrap = import ./bootstrap.nix;
    ghc = import ./ghc.nix;
    ghc-packages = import ./ghc-packages.nix;
    windows = import ./windows.nix;
    armv6l-linux = import ./armv6l-linux.nix;
    musl = import ./musl.nix;
    tools = import ./tools.nix;
    emscripten = import ./emscripten.nix;
    nix-prefetch-git-minimal = import ./nix-prefetch-git-minimal.nix;
    eval-on-current = import ./eval-on-current.nix;
    eval-on-build = import ./eval-on-build.nix;
  };

  composeExtensions = f: g: final: prev:
    let
      fApplied = f final prev;
      prev' = prev // fApplied;
    in fApplied // g final prev';

  ordered = with overlays; [
    # Hide nixpkgs haskell and haskellPackages from the haskell-nix overlays.
    # This should prevent us inadvertantly depending on them.
    (_: prev: {
      haskell = { };
      haskellPackages = { };
      haskell-nix-prev = prev;
    })
    wine
    haskell
    hackage-quirks
    bootstrap
    ghc
    ghc-packages
    windows
    armv6l-linux
    musl
    tools
    emscripten
    nix-prefetch-git-minimal
    # Restore nixpkgs haskell and haskellPackages
    (_: prev: { inherit (prev.haskell-nix-prev) haskell haskellPackages; })
  ];
  combined = builtins.foldl' composeExtensions (_: _: { })
    (ordered ++ [overlays.eval-on-current]);
  combined-eval-on-build = builtins.foldl' composeExtensions (_: _: { })
    (ordered ++ [overlays.eval-on-build]);
in overlays // { inherit combined combined-eval-on-build; }
