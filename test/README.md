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
make test-iserv-lifecycle ISERV_PROXY=/path/to/bin/iserv-proxy
make test-qemu-linux-user SYSTEM=x86_64-linux
```

The wrapper checks use a recording proxy and require Nix and Python.
The POSIX lifecycle checks use the built native proxy and a fake interpreter.
They cover normal and slow Shutdown, nonzero exit and SIGSEGV, exit while
GHC is idle, descendants retaining stdout, TERM-resistant cleanup,
descriptor isolation, GHC disconnect, and cancellation before and during
Shutdown. Every fixture has a deadline and checks for surviving owned
processes before fallback cleanup. Run with both threaded and non-threaded
proxy builds when changing process handling.

The QEMU checks compile a static AArch64 guest with a cross C compiler.
They check code-page updates, four concurrent writers over 256 rounds,
and require real read-only and unmapped writes
to terminate with signal status 139 within a deadline. `QEMU=/nix/store/…`
selects an already-built emulator. The default uses the same patched QEMU
selection as the Linux cross-TH overlay.
The test driver also exposes `qemu-linux-user.run` on Linux with supported
QEMU versions, so the checks enter the existing CI test matrix.

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
and `allowSubstitutes = false` make the tests execute on the selected
builder instead of reusing a cached success. Retain the output paths and
verify which physical builder executed each test when using remote builders.

For the verified crash and hang mechanism, see
[the cross-compilation investigation](../docs/tutorials/cross-compilation.md#diagnosing-template-haskell-failures-on-rosetta-builders).
