[
    # Hide nixpkgs haskell and haskellPackages from the haskell-nix overlays.
    # This should prevent us inadvertantly depending on them.
    (_: super: { haskell = {}; haskellPackages = {}; haskell-nix-super = super; })
    (import ./release-19.03.nix)
    #(import ./ghcjs-asterius-triple.nix)
    #(import ./python.nix)
    (import ./haskell.nix)
    (import ./bootstrap.nix)
    (import ./ghc.nix)
    (import ./ghc-packages.nix)
    (import ./windows.nix)
    (import ./armv6l-linux.nix)
    # Restore nixpkgs haskell and haskellPackages
    (_: super: { inherit (super.haskell-nix-super) haskell haskellPackages; })
]