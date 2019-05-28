## May 29, 2019
  * Added `shellFor` function to package set.

## May 28, 2019
  * Added `snaphots` and `haskellPackages` attributes to the
    Haskell.nix top-level.

## May 22, 2019
  * Add the `cleanSourceHaskell` utility function to the Haskell.nix
    top-level.
  
## May 21, 2019
  * Add the `callCabalProjectToNix` function, which uses "import from
    derivation" (IFD) so that nix-tools doesn't need to be run
    manually.
  * The `hackage.nix` update process has changed, so that Cabal index
    state hashes are also included in the generated repo.

## May 20, 2019
  * Remove Travis CI in favour of Buildkite.

## May 17, 2019
  * Add the `callStackToNix` function, which uses "import from
    derivation" (IFD) so that `stack-to-nix` doesn't need to be run
    manually.
  
## Mar 15, 2019
  * overlays was renamed to extras (#79)
    See docs/migration.md
