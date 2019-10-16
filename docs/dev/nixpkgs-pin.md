# Haskell.nix Nixpkgs Pin

Haskell.nix contains a Nixpkgs pin in
[`nixpkgs/github.json`](https://github.com/input-output-hk/haskell.nix/blob/master/nixpkgs/github.json).
This is the version of Nixpkgs used for builds of `nix-tools` and
running the tests.

To use haskell.nix the `config` and `overlays` need to be applied to
Nixpkgs.  Users should probably pin a suitable version of nixpkgs
although, although things might not work for them if their Nixpkgs version is
too different.

We aim to keep this pin somewhere on the channel of the **NixOS latest
stable release**. That is currently 19.03. So:

```
nix-prefetch-git https://github.com/NixOS/nixpkgs-channels refs/heads/nixos-19.03
```

Keep the URL in `github.json` pointing at
<https://github.com/NixOS/nixpkgs>. Case matters because the Hydra
trusted URL whitelist is case-sensitive.
