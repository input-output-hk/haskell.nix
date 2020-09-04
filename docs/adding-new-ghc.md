# Adding a new GHC version to haskell.nix

## Update `overlays/bootstrap.nix`

Each ghc version is defined in this file.  Duplicate one of the existing
ghc version definitions and replace the version numbers.  Make sure
you update the `spec.sha256` or the other versions source will be used.
Check the LLVM version that should be used in the
[ghc wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/backends/llvm/installing).

## Update the list of cached GHC versions in `ci.nix`

## Update [supported ghc versions](supported-ghc-versions.md) document

## Add the materialized files

In the haskell.nix repo run:

```
mkdir materialized/ghc884
nix-build scripts/check-compiler-materialization --argstr compiler-nix-name ghc884
```

The `nix-build` command will fail with something like:

```
Materialized nix used for dummy-data-x86_64-unknown-linux-musl-ghc-8.10.1 incorrect. To fix run: /nix/store/wnwpyrhv4nxgyljz3f20gdpspjxvm7h4-updateMaterialized
```

Run the `updateMaterialized` script and repeat the `nix-build` until it no longer fails.
If the failure is not a problem with materialization and no `updateMaterialized` script
is provided then you may need to fix the failure another way or (if it only relates to
one of the cross compilers) modify `scripts/check-compiler-materialization/default.nix`
so that it skips that compiler.
