# `haskell.nix` is infrastructure for building Haskell packages with Nix

`haskell.nix` can automatically translate your Cabal or Stack project and
its dependencies into Nix code.

## Documentation

- [Introduction](https://input-output-hk.github.io/haskell.nix/index.html)
- [Getting Started](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started)
- [Troubleshooting](https://input-output-hk.github.io/haskell.nix/troubleshooting)
- Explore the documentation from there to find further topics.

## Help! Something isn't working

The #1 problem that people have when using `haskell.nix` is that they find themselves building GHC.
This should not happen, but you *must* follow the `haskell.nix` setup instructions properly to avoid it.
If you find this happening to you, please check that you have followed the 
[getting started instructions](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started#setting-up-the-binary-cache) and
consult the corresponding [troubleshooting section](https://input-output-hk.github.io/haskell.nix/troubleshooting#why-am-i-building-ghc).

The troubleshooting documentation also contains some help for other common issues. If you're still stuck open an issue.

## Related repos

The `haskell.nix` repository contains the runtime system for building
Haskell packages in Nix. It depends on other repos, which are:

- [`hackage.nix`](https://github.com/input-output-hk/hackage.nix) — the latest contents of the [Hackage](https://hackage.haskell.org/) databases, converted to Nix expressions.

- [`stackage.nix`](https://github.com/input-output-hk/stackage.nix) — all of the [Stackage](https://www.stackage.org/) snapshots, converted to Nix expressions.

