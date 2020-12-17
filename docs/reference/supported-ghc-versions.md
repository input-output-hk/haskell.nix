# Supported GHC Versions

This table shows which versions of GHC are defined, built by CI, and tested. 
If you use a combination of nixpkgs version and GHC version which is built by our CI, you should get cache hits from our cache.

The "nixpkgs versions" refer to the versions that `haskell.nix` provides; if you use your own version of nixpkgs you will likely not get cache hit

| Nixpkgs version | GHC version   | Defined? | Built? | Tested? |
| --------------- | ------------- | -------- | ------ | ------- |
| 20.03           | 8.4.4         | Y        | N      | N       |
| 20.03           | 8.6.{1,2,3,4} | Y        | N      | N       |
| 20.03           | 8.6.5         | Y        | Y      | Y       |
| 20.03           | 8.8.{1,2,3}   | Y        | N      | N       |
| 20.03           | 8.8.4         | Y        | N      | N       |
| 20.03           | 8.10.1        | Y        | N      | N       |
| 20.03           | 8.10.2        | Y        | N      | N       |
| 20.09           | 8.4.4         | Y        | N      | N       |
| 20.09           | 8.6.{1,2,3,4} | Y        | N      | N       |
| 20.09           | 8.6.5         | Y        | Y      | Y       |
| 20.09           | 8.8.{1,2}     | Y        | N      | N       |
| 20.09           | 8.8.3         | Y        | Y      | N       |
| 20.09           | 8.8.4         | Y        | Y      | Y       |
| 20.09           | 8.10.1        | Y        | Y      | N       |
| 20.09           | 8.10.2        | Y        | Y      | Y       |

See [ci.nix](https://github.com/input-output-hk/haskell.nix/blob/master/ci.nix) for the source of truth about what is built and tested.

See [overlays/bootstrap.nix](https://github.com/input-output-hk/haskell.nix/blob/master/overlays/bootstrap.nix) for a list of all the valid `compiler-nix-names`.

See also: [Instructions on adding new GHC versions](../dev/adding-new-ghc.md).
