# `haskell.nix` is infrastructure for building Haskell packages with Nix

[![](https://badge.buildkite.com/d453edcd29bd2f8f3f3b32c9b7d6777a33773d9671c37a6ccc.svg?branch=master)](https://buildkite.com/input-output-hk/haskell-dot-nix)
[![](https://img.shields.io/buildkite/c8d5a20d3ff0f440f82adb9190b43c16c91e5e47e8adfa867a/master.svg?label=nightly%20updates)](https://buildkite.com/input-output-hk/haskell-dot-nix-nightly-updates)

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

The troubleshooting documentation also contains some help for other common issues.
Please give it a read before asking on IRC or opening an issue.

If you're still stuck, join the [#haskell.nix](https://www.irccloud.com/invite?channel=%23haskell.nix&hostname=irc.libera.chat&port=6697&ssl=1) channel on [irc.libera.chat](https://libera.chat/), or open an issue.

## Related repos

The `haskell.nix` repository contains the runtime system for building
Haskell packages in Nix. It depends on other repos, which are:

- [`nix-tools`](https://github.com/input-output-hk/nix-tools) — provides the programs for generating Nix expressions from  Haskell projects.

- [`hackage.nix`](https://github.com/input-output-hk/hackage.nix) — the latest contents of the [Hackage](https://hackage.haskell.org/) databases, converted to Nix expressions.

- [`stackage.nix`](https://github.com/input-output-hk/stackage.nix) — all of the [Stackage](https://www.stackage.org/) snapshots, converted to Nix expressions.

