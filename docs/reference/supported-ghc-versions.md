# Supported GHC Versions


The following GHC versions are defined in `haskell.nix` (i.e. there is a derivation for them even if it isn't necessarily cached):
- 8.4.4
- 8.6.{1,2,3,4,5}
- 8.8.{1,2,3,4}
- 8.10.{1,2,3,4}

The following table shows the nixpkgs/GHC versions which are built by CI, and which of those are tested.
If you use a combination of nixpkgs version and GHC version which is in this table, you should get cache hits from our cache.
The "nixpkgs versions" refer to the versions that `haskell.nix` provides; if you use your own version of nixpkgs you will likely not get cache hit.

| Nixpkgs version  | GHC version | Tested? |
|------------------|-------------|---------|
| 20.03            | 8.6.5       | Y       |
| 20.09            | 8.6.5       | Y       |
| 20.09            | 8.8.4       | Y       |
| 20.09            | 8.10.4      | Y       |
| nixpkgs-unstable | 8.6.5       | Y       |
| nixpkgs-unstable | 8.8.4       | Y       |
| nixpkgs-unstable | 8.10.4      | Y       |

See [ci.nix](https://github.com/input-output-hk/haskell.nix/blob/master/ci.nix) for the source of truth about what is built and tested.

See [overlays/bootstrap.nix](https://github.com/input-output-hk/haskell.nix/blob/master/overlays/bootstrap.nix) for a list of all the valid `compiler-nix-names`.

See also: [Instructions on adding new GHC versions](../dev/adding-new-ghc.md).
