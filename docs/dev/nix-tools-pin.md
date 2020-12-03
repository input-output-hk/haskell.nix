## How to update [`nix-tools`](https://github.com/input-output-hk/nix-tools)

1. Use niv to update the sources.json:

   ```
   niv update nix-tools
   ```


2. If `nix-tools.cabal` or `plan-to-nix` have changed, check the
   materialized files for each of the compiler nix name in
   `ls -d materialized/ghc*/nix-tools` with:

   ```
   nix-build scripts/check-compiler-materialization --argstr compiler-nix-name ghc884
   ```
