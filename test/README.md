### Haskell infrastructure test cases

To build the test cases, run from this directory:

    nix-build --no-out-link default.nix

To run all tests (includes impure tests), use the script:

    ./tests.sh


#### Generated code

If you change the test Cabal files or need to regenerate the code with
nix-tools, then see `regen.nix`. Run it like this:

    $(nix-build --no-out-link regen.nix)
