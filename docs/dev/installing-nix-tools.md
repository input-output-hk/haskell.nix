## Installing `nix-tools`

To build the latest `nix-tools` and store the result at `./nt`, run:

```bash
nix build -f https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz pkgs.haskell-nix.nix-tools.ghc884 --out-link nt
```

If you would like to then install `nix-tools` into your profile, run:

```bash
nix-env -i ./nt
```

## Optional: Installing via [Haskell.nix][] source

The [Haskell.nix][] and `nix-tools` source will be useful if you would
like to contribute improvements, or read the source code to fully
understand something that the documentation doesn't cover.

```bash
git clone https://github.com/input-output-hk/nix-tools
git clone https://github.com/input-output-hk/haskell.nix
cd haskell.nix
nix build -f . pkgs.haskell-nix.nix-tools.ghc884 --arg sourcesOverride '{ nix-tools = ../nix-tools; }' --out-link nt
```

[haskell.nix]: https://github.com/input-output-hk/haskell.nix
