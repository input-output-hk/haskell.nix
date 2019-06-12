# `ghcWithPackages` wrapper removal

The current [Nixpkgs Haskell infrastructure][nixpkgs-haskell] and `haskell.nix` both
provide a `ghcWithPackages` derivation which contains shell script
wrappers that wrap `ghc` and `ghc-pkg`.

In the Nixpkgs Haskell infrastructure, the wrapper scripts are used
for building Haskell packages. However, in `haskell.nix`, the wrappers
are only used for development environments.

The wrapper scripts provide a `ghc` command that "knows" about the
package set and has all Haskell package dependencies available to it.

We would like to remove the wrapper scripts, but it's currently not
possible to configure all build tools using environment variables
alone.

## Plain `ghc`

When using `ghc` or `ghci` by itself, the `GHC_ENVIRONMENT` variable
can point to a configuration file containing an exact package
set. This works quite well.

## `ghc-pkg`

The package tool `ghc-pkg` does not recognize `GHC_ENVIRONMENT`, but
does recognize a `GHC_PACKAGE_PATH` pointing to a `package.conf.d`.

This works well. However, the `cabal` command will refuse to start if
`GHC_PACKAGE_PATH` is set.

## `Setup.hs`

When invoking `Setup.hs configure`, the package database is provided
with the `--package-db` argument and exact dependencies in the package
set can be provided as `--dependency` arguments.

The `haskell.nix` component builder uses `Setup.hs` with these
command-line options to build Haskell packages.

## `cabal new-build`

Cabal-install will observe the `CABAL_CONFIG` environment variable,
which points to a cabal config file. This config file can provide a
`package-db` value, but it can't specify exact versions of packages.

Cabal is designed to solve dependencies, not simply take the package
set which is given to it.

Therefore, `cabal` does not use `GHC_ENVIRONMENT`, but instead creates
its own environment file. It will not accept `--dependency` arguments.

As far as I know, the best way to force `cabal` to take a pre-computed
package set is to use a `new-freeze` file. However there is no
environment variable (or config file entry) which can specify a path
to a freeze file.

Specifying a `package-db` path in the cabal config file is not enough
for it to successfully resolve dependencies.

As mentioned before, `cabal` does not work when `GHC_PACKAGE_PATH` is
set. The best way to work around this is to wrap `ghc` and `ghc-pkg`
in shell scripts.


[nixpkgs-haskell]: https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure
