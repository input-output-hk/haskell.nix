# Getting started with Hix

Hix is a command line tool that provides an easy way to add haskell.nix
support to existing haskell projects.

You will need `nix` installed and in you `PATH` with nix in PATH with
`experimental-features = [ "nix-command" "flakes" ];` configured.
See https://nixos.wiki/wiki/Flakes for details.

## Using `hix init` and `nix`

The `hix init` command adds a `flake.nix` and `nix/hix.nix` file.
After that the project can be used with regular `nix` tools.

For instance to run `cabal build` on the `hello` package from hackage:

```shell
cabal unpack hello
cd hello-1.0.0.2
nix run "github:input-output-hk/haskell.nix#hix" -- init
nix develop
cabal build
```

To view the contents of the flake run:

```shell
nix flake show
```

To build a component with nix:

```shell
nix build .#hello:exe:hello
```

To build and run a component:

```shell
nix run .#hello:exe:hello
```

## Installing Hix

To use the other Hix features first install Hix with:

```shell
nix-env -iA hix -f https://github.com/input-output-hk/haskell.nix/tarball/master
```

To update run to the latest version run:

```shell
hix update
```

## Using `hix develop`, `hix flake`, `hix build` and `hix run`

These commands work the same as the `nix` versions
without using the `flake.nix`.  Instead a boiler
plate haskell.nix flake.nix file is added to
`.hix-flake/flake.nix` and used from there.

The is can be useful if the project already includes a
`flake.nix` or if you do not intend to maintain one.


Then all of these should work without the need to
run `hix init`:

```shell
hix develop
hix flake show
hix build .#hello:exe:hello
hix run .#hello:exe:hello
```

## Using `hix-shell` and `hix-build`

These commands behave like `nix-build` and `hix-shell`
would if a boiler plate `default.nix` and `shell.nix`
we present.

```shell
hix-shell --run 'cabal build all'
hix-build -A hsPkgs.hello.components.exes.hello
```
