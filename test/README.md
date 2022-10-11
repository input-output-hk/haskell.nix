### Haskell infrastructure test cases

To build the test cases, run from the `test` directory:

```shell
nix-build --no-out-link default.nix
```

To run all tests (includes impure tests), use the script:

```shell
./tests.sh
```

#### Generated code

If you change the test Cabal files or need to regenerate the code with
nix-tools, then see `regen.nix`. Run it like this:

```shell
$(nix-build --no-out-link regen.nix)
```