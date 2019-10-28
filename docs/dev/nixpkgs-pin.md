# Haskell.nix Nixpkgs Pin

Haskell.nix contains a Nixpkgs pin in
[`nixpkgs/github.json`](https://github.com/input-output-hk/haskell.nix/blob/master/nixpkgs/github.json).
This is the version of Nixpkgs used for builds of `nix-tools` and
running the tests.

To use haskell.nix the `config` and `overlays` need to be applied to
Nixpkgs.  Users should probably pin a suitable version of nixpkgs, although things might not work for them if their Nixpkgs version is
too different.

We aim to keep this pin somewhere on a channel of the **Nixpkgs latest
stable release**. That is currently 19.09.

We also execute tests on MacOS (darwin). The darwin channel is usually
behind the NixOS channel. So we choose the `nixpkgs-19.09-darwin`
channel:

```
nix-prefetch-git https://github.com/NixOS/nixpkgs-channels refs/heads/nixpkgs-19.09-darwin
```

Keep the URL in `github.json` pointing at
<https://github.com/NixOS/nixpkgs>. Case matters because the Hydra
trusted URL whitelist is case-sensitive.
