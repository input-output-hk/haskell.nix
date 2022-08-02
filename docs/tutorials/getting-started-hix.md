# Getting started with Hix

Hix is a command line tool that provides an easy way to add haskell.nix
support to existing haskell projects.

The `hix init` command adds a `flake.nix` and `nix/hix.nix` file.
After that the project can be used with regular `nix` tools.

For instance to run `cabal build` on the `hello` package from hackage:

```bash
cabal unpack hello
cd hello-1.0.0.2
nix run github:input-output-hk/haskell.nix#hix -- init
nix develop
cabal build
```
