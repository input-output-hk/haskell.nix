# Haskell.nix Nixpkgs Pin

Haskell.nix contains several Nixpkgs pins imanaged by `niv` in
`nix/sources.json`.

These are used in testing various versions of nixpkgs.

To use haskell.nix the `config` and `overlays` need to be applied to
Nixpkgs.  Users should probably pin a suitable version of nixpkgs, although things might not work for them if their Nixpkgs version is
too different.

We aim to keep this pin somewhere on a channel of the **Nixpkgs latest
stable release**. That is currently 20.09.

We also execute tests on MacOS (darwin). The darwin channel is usually
behind the NixOS channel. So we follow the `nixpkgs-20.09-darwin`
channel.
