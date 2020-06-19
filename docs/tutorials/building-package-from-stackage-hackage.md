
# Build a specific package from hackage or a [stackage][] lts or nightly set.

To build a package, say [lens][], from a stackage snapshot, say [lts-13.28][],
you could run
```bash
nix build '(with import <nixpkgs> (import ./. {}).nixpkgsArgs; haskell-nix.snapshots."lts-13.28").lens.components.library'
```
which would build the [lens][] library component from the lens package as fixed
by the [lts-13.28][] stackage snapshot.

## Specific version

To build any package from hackage, say [lens][], in version, say 4.17.1, you
could run
```bash
nix build '(with import <nixpkgs> (import ./. {}).nixpkgsArgs; (haskell-nix.hackage-package { name = "lens"; version = "4.17.1"; })).components.library'
```
which would build the [lens][] library component from the [lens-4.17.1][] package
from hackage.  

## Pinning hackage index

The dependencies would be solved against the most recent 
[hackage-index-state][] that comes via the [hackage.nix][] pin with your
[haskell.nix][] checkout.  A specific one can be specified as well:
```bash
nix build '(with import <nixpkgs> (import ./. {}).nixpkgsArgs; (haskell-nix.hackage-package { name = "lens"; version = "4.17.1"; index-state = "2019-07-14T00:00:00Z"; })).components.library'
```
which would use the hackage index as of `2019-07-14T00:00:00Z` to produce a build plan
for the [lens-4.17.1][] package.
   
[stackage]: https://stackage.org
[hackage.nix]: https://github.com/input-output-hk/hackage.nix
[lts-13.28]: https://www.stackage.org/lts-13.28
[lens]: https://hackage.haskell.org/package/lens
[lens-4.17.1]: https://hackage.haskell.org/package/lens-4.17.1
[hackage-index-state]: https://github.com/input-output-hk/hackage.nix/blob/master/index-state-hashes.nix