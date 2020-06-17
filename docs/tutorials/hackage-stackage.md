# Bumping Hackage and Stackage snapshots

`haskell.nix` relies on some generated data providing information about packages in Hackage and Stackage snapshots.
These are kept in [`hackage.nix`](https://github.com/input-output-hk/hackage.nix) and [`stackage.nix`](https://github.com/input-output-hk/stackage.nix) respectively.
If your project depends on a Hackage package, then the `hackage.nix` revision used must be new enough to contain that, and likewise for Stackage snaphots and `stackage.nix`.

## Updating and pinning `hackage.nix` and `stackage.nix`

`haskell.nix` pins particular revisions of these repositories internally, both for our own usage in testing, and so that users have a sensible default when getting started. 
These revisions are updated nightly, so you can get newer revisions of `hackage.nix` and `stackage.nix` by updating your revision of `haskell.nix` itself.

However, this exposes you to changes in `haskell.nix` which you may not want, such as changes that force compiler rebuilds, or the occasional bug.
Instead, you can pin `hackage.nix` and `stackage.nix` independently. For example:

```nix
let 
  # You can use a tool like `niv` to manage this boilerplate
  hackageSrc = builtins.fetchTarball https://github.com/input-output-hk/hackage.nix/archive/master.tar.gz;
  stackageSrc = builtins.fetchTarball https://github.com/input-output-hk/stackage.nix/archive/master.tar.gz;
  haskellSrc = builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz;

  haskellNix = import haskellSrc {
    # This allows you to override the pins used by `haskell.nix` internally
    sourcesOverride = {
      hackage = hackageSrc;
      stackage = stackageSrc;
    };
  };
in ...
```

This way you can change the revisions of `hackage.nix` and `stackage.nix` are fetched from without changing `haskell.nix`.

However, bear in mind that Stackage refers to Hackage, so your Stackage should never be newer than Hackage.

### Pinning the Hackage version

Sometimes you might want to use a chosen version of Haskell.nix with a
recent update of Hackage or Stackage. This can be done with JSON pins:

```bash
nix-prefetch-git https://github.com/input-output-hk/hackage.nix | tee hackage-src.json
nix-prefetch-git https://github.com/input-output-hk/stackage.nix | tee stackage-src.json
```

The resulting JSON files will correspond to the latest revision of
[hackage.nix][] and [stackage.nix][]. See
[Architecture](../architecture.md) for more information about how
these Git repositories correspond to the actual Hackage and Stackage.

[hackage.nix]: https://github.com/input-output-hk/hackage.nix
[stackage.nix]: https://github.com/input-output-hk/stackage.nix

```nix
{ pkgs ? import <nixpkgs> (haskellNixArgs // { overlays = haskellNixArgs.overlays ++ [
  (self: super: {
    haskell-nix = super.haskell-nix // {
      hackageSourceJSON  = ./hackage-src.json;
      stackageSourceJSON = ./stackage-src.json;
    };
  })]; })
, haskellNixArgs ? (import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) {}).nixpkgsArgs
}:
  pkgs
```