# Supported GHC Versions

The following versions of GHC built on the CI servers and should be included
in the cache (for the default haskell.nix `nixpkgs`).

* 8.6.5 `compiler-nix-name = "ghc865";`
* 8.8.3 `compiler-nix-name = "ghc883";`
* 8.8.4 `compiler-nix-name = "ghc884";`
* 8.10.1 `compiler-nix-name = "ghc8101";`
* 8.10.2 `compiler-nix-name = "ghc8102";`

Full test suite is run against 8.6.5, 8.8.4 and 8.10.2.

See [ci.nix](https://github.com/input-output-hk/haskell.nix/blob/master/ci.nix)
for the list of tested GHC versions.

The following GHC versions are not included in CI and will not be cached:

* 8.4.4 `compiler-nix-name = "ghc844";`
* 8.6.1 `compiler-nix-name = "ghc861";`
* 8.6.2 `compiler-nix-name = "ghc862";`
* 8.6.3 `compiler-nix-name = "ghc863";`
* 8.6.4 `compiler-nix-name = "ghc864";`
* 8.8.1 `compiler-nix-name = "ghc881";`
* 8.8.2 `compiler-nix-name = "ghc882";`

See [overlays/bootstrap.nix](https://github.com/input-output-hk/haskell.nix/blob/master/overlays/bootstrap.nix)
for a list of all the valid `compiler-nix-names`.

See also:

* [Instructions on adding new ghc versions](adding-new-ghc.md).
