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

#### Cross-Template-Haskell regression checks

From the repository root:

```shell
make check test-linux-cross-wrapper
make test-qemu-linux-user SYSTEM=x86_64-linux
```

The wrapper checks use a recording proxy and require Nix and Python.
`make verify` runs the syntax, dependency, emulator-selection, and wrapper
checks without building QEMU.

The QEMU checks compile a static AArch64 guest with a cross C compiler.
They check code-page updates, four concurrent writers over 256 rounds,
and require real read-only and unmapped writes
to terminate with signal status 139 within a deadline. `QEMU=/nix/store/…`
selects an already-built emulator and records it as a Nix dependency.
The default uses the same QEMU selection as the Linux cross-TH overlay:
patched on x86_64 Linux with QEMU 9.1–11, unchanged on native ARM64.
The test driver exposes `qemu-linux-user.run` on supported x86_64 Linux
build hosts, so the checks enter the existing CI test matrix. The guest
compile uses `buildPackages.coreutils` for build-host `timeout`.

For host signal diagnostics in an ARM64 nix-linux-builder VM:

```shell
make probe-rosetta-signals SYSTEM=x86_64-linux
make test-rosetta-signals SYSTEM=aarch64-linux
```

The first command records Rosetta behavior in `results.tsv` and per-case
logs even when the host is nonconforming. A successful **probe** build only
means the evidence was saved. `make test-rosetta-signals` enforces correct
signal behavior and currently fails under the affected Rosetta runtime.
The native ARM64 control should pass in the same VM. `preferLocalBuild`
requests a local build and `allowSubstitutes = false` disables substitution;
neither forces an output already present in the store to run again.
To rerun an existing output, use `nix-build --check` or `nix build --rebuild`.
Retain the logs and verify which physical builder executed the tests.
A cached Hydra result also does not prove execution on the affected builder.

For the verified crash and hang mechanism, see
[the cross-compilation investigation](../docs/tutorials/cross-compilation.md#diagnosing-template-haskell-failures-on-rosetta-builders).
