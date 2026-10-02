# Cross compilation

Cross compilation of Haskell projects involves building a version of
GHC that outputs code for the target platform, and providing builds of
all library dependencies for that platform.

First, understand how to cross-compile a normal package from
Nixpkgs. Matthew Bauer's [Beginners' guide to cross compilation in
Nixpkgs][bauer] is a useful resource.

[bauer]: https://matthewbauer.us/blog/beginners-guide-to-cross.html


Using an example from the guide, this builds GNU Hello for a Raspberry
Pi:

    nix build -f '<nixpkgs>' pkgsCross.raspberryPi.hello

We will use the same principle in [Haskell.nix][] — replacing the normal
package set `pkgs` with a cross-compiling package set
`pkgsCross.raspberryPi`.

### Raspberry Pi example

This is an example of using [Haskell.nix][] to build the [Bench][]
command-line utility, which is a Haskell program.

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  native = haskellNix { inherit pkgs; };
in
  native.haskellPackages.bench.components.exes.bench
```

Now switch the package set as in the previous example:

```nix
{ pkgs ? import <nixpkgs> {} }:
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  raspberryPi = haskellNix { pkgs = pkgs.pkgsCross.raspberryPi; };
in
  raspberryPi.haskellPackages.bench.components.exes.bench
```

You should be prepared for a long wait because it first needs to build
GHC, before building all the Haskell dependencies of [Bench][]. If all
of these dependencies compiled successfully, I would be very surprised!

> **Hint:**
>
> The above example won't build, but you can try and see, if you like.
> It will fail on [clock-0.7.2](http://hackage.haskell.org/package/clock-0.7.2),
> which needs a patch to build.

To fix the build problems, you must add extra configuration to the
package set. Your project will have a [`mkStackPkgSet`](../reference/library.md#mkstackpkgset) or
[`mkCabalProjectPkgSet`](../reference/library.md#mkcabalprojectpkgset). It is there where you must add
[module options](../reference/modules.md) for setting compiler flags, adding patches, and so on.

> **Note:**
>
> Note that `haskell.nix` will automatically use `qemu` to emulate the target
> when necessary to run Template Haskell splices.

### Static executables with Musl libc

Another application of cross-compiling is to produce fully static
binaries for Linux. For information about how to do that with the
[Nixpkgs Haskell infrastructure][nixpkgs] (not [Haskell.nix][]), see
[nh2/static‑haskell‑nix][nh2]. Vaibhav Sagar's linked
[blog post][vaibhav] is also very informative.


```nix
{ pkgs ? import <nixpkgs> {} }:
let
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz);
  musl64 = haskellNix { pkgs = pkgs.pkgsCross.musl64; };
in
  musl64.haskellPackages.bench.components.exes.bench
```

This example will build [Bench][] linked against Musl libc. However
the executable will still be dynamically linked. To get fully static
executables you must add package overrides to:

1. Disable dynamic linking
2. Provide static versions of system libraries. (For more details, see
   [Vaibhav's article][vaibhav]).

```nix
{
  packages.bench.components.exes.bench.configureFlags =
    lib.optionals stdenv.hostPlatform.isMusl [
      "--disable-executable-dynamic"
      "--disable-shared"
      "--ghc-option=-optl=-pthread"
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-L${gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${zlib.static}/lib"
    ];
}
```

> **Note:** Licensing
>
> Note that if copyleft licensing your program is a problem for you,
> then you need to statically link with `integer-simple` rather than
> `integer-gmp`. However, at present, [Haskell.nix][] does not provide
> an option for this.


### How to cross-compile your project

Set up your project Haskell package set.

```nix
{{#include cross-compilation/default.nix}}
```

Apply that package set to the Nixpkgs cross package sets that you are
interested in.

We are going to expand the `pkgs.pkgsCross` shortcut to be more
explicit.

```nix
let
  pkgs = import <nixpkgs> {}
in {
  shortcut = pkgs.pkgsCross.SYSTEM;
  actual = import <nixpkgs> { crossSystem = pkgs.lib.systems.examples.SYSTEM; };
}
```

In the above example, for any `SYSTEM`, `shortcut` and `actual` are
the same package set.

```nix
{{#include cross-compilation/release.nix}}
```

Try to build it, and apply fixes to the `modules` list, until there
are no errors left.


### Diagnosing Template Haskell failures on Rosetta builders

The execution chain for Linux cross compilation is GHC → `iserv-wrapper`
→ `iserv-proxy` → QEMU → the target interpreter. A target signal and a
later Nix timeout can have separate causes. Preserve the process tree and
wait channels before terminating a stalled build. Enable core dumps for a
bounded reproduction; QEMU's “core dumped” message alone does not prove
that it wrote a core file.

Investigation of [build 2014157](https://ci.zw3rk.com/build/2014157) on
2026-10-02 isolated the failure to x86_64 QEMU running under Rosetta in the
ARM64 Linux guest of nix-linux-builder. The failing job was
`x86_64-linux.unstable.ghc9141.aarch64-android-prebuilt.tests.th-dlls.build-profiled`.
The exact cached compiler, proxy, target interpreters, and package inputs
were reused in each reproduction. The two compilation passes behaved as
follows:

| Execution environment | Normal pass | Profiled pass |
| --- | --- | --- |
| Native x86_64 Linux, original QEMU 11.0.1 | Completed | Completed |
| ARM64 VM, x86_64 QEMU 11.0.1 under Rosetta | Completed | Target SIGSEGV; QEMU remained alive |
| Same ARM64 VM, patched x86_64 QEMU 11.0.1 under Rosetta | Completed | Completed |
| Same ARM64 VM, native ARM64 QEMU 11.0.1 | Completed | Completed |
| Same ARM64 VM, static ARM64 interpreter executed directly | Completed | Completed |

The patched reproduction also completed installation and fixup with exit
code 0. It retained the original proxy, interpreters, and shell wrapper
behavior; only the emulator changed. The successful run used a 240-second
wall-clock bound. An initial 150-second limit stopped before completion.
The immutable evidence is
`/nix/store/87dzdfs1sw5wihg03c5bh0h2k7py3q1v-haskell-nix-patched-qemu-instrumented-th-repro`
(`repro.log`, `processes.log`, and `exit-code`).
The diagnostic derivation saves evidence even when its inner build fails;
inspect `exit-code` rather than treating a successful diagnostic Nix build
as a successful package build.

The native x86_64 `nix build --rebuild` completed compilation and
installation, then failed Nix's comparison of output contents. That
separate reproducibility failure is not evidence of a Template Haskell
crash. A cached Hydra success also does not establish that the Darwin
builder executed successfully: some green jobs reuse outputs while their
recorded Darwin attempts failed.

Two host signal defects explain the observed crash and hang:

1. Rosetta reports `SEGV_MAPERR` for writes to existing anonymous pages
   protected with `mprotect`. Native ARM64 reports `SEGV_ACCERR` for the
   same tests in the same VM. This occurs with resident and nonresident
   pages and with read-only, read/execute, and inaccessible protections.
   The x86 fault context correctly reports a write (`REG_ERR=6`), but its
   `si_code` is wrong. QEMU's [signal handler](https://github.com/qemu/qemu/blob/v11.0.1/linux-user/signal.c)
   uses `SEGV_ACCERR` to recognize writes to pages it protected for
   translated code. The profiled interpreter core stops at the first
   `memcpy` store in `ocGetNames_ELF`, loading `HSbase-4.22.0.0-inplace.p_o`;
   QEMU's guest memory map lists the destination as writable.
2. Rosetta loses an x86 process's self-sent `SIGSEGV` when that signal is
   blocked. QEMU's fatal-signal path sends the signal while blocked, then
   calls `sigsuspend`. In the reproduction QEMU remained in `sigsuspend`,
   the proxy waited for a reply, and GHC waited for the proxy. Unblocking
   `SIGSEGV` **before** sending it makes the small host probe terminate
   with status 139. Unblocking it after sending it does not recover the
   lost signal. Native ARM64 correctly preserves and delivers it.

The stack-size warning is separate. Nix emits it after a failed
`setrlimit(RLIMIT_STACK)` and continues. Matched successful nonprofiled
jobs contain the same warning, and older QEMU 10.2.2 failures have no such
warning. The VM and both host architectures report 4096-byte pages;
page-size mismatch was tested and rejected. Android, LLVM, and QEMU 11
are not required for this recurring CI signature.

The proxy also needs independent lifecycle guards. Child exit must wake a
proxy waiting on either GHC or the interpreter, and cleanup must have a
wall-clock bound even when the child ignores TERM or a descendant holds
stdout open. The shell wrapper must `exec` the proxy so GHC owns its PID.
These guards cannot detect a QEMU process that remains alive in
`sigsuspend`; fixing the host signal path is required for that case.

The Linux cross-TH overlay now selects a user-only QEMU for the required
target and applies two signal fixes for QEMU 9.1 through 11:

- For x86 hosts, accept a write reported as `SEGV_MAPERR` only when QEMU's
  page metadata permits the guest write and `mincore` confirms that the
  host page exists. Then use QEMU's existing code-page unprotect path.
  Read-only guest pages and missing host mappings remain faults. Check
  metadata under the mmap lock and allow another writer to have already
  unprotected the page.
- Unblock the fatal signal before sending it to the emulator itself.

The patch applies to the pinned QEMU releases 9.1.3, 9.2.4, 10.1.5,
10.2.4, and 11.0.1. Runtime validation used 11.0.1. Older releases, QEMU
12 or later, and Darwin-host QEMU retain their original selection. Review
and extend this version boundary when updating the QEMU pin.

The small guest regressions check code changes, four concurrent writers
over 256 rounds, and real read-only and unmapped writes. The original
x86 emulator under Rosetta failed all four cases; the patched emulator
passed all four, as did native ARM64 QEMU in the same VM. The patched
results are in
`/nix/store/1vf1mjrqkgb3gl5whiy7b2a3vf5w59vh-qemu-linux-user-x86_64-linux`.
The patched binary was compiled with an ARM64-hosted x86 cross compiler
because the native x86 package build did not finish within its bound.
Its temporary validation package supplies the cross compiler's runtime
library for glibc's `pthread_exit`. The complete default QEMU package
build was not validated locally.

Proxy lifecycle fixtures passed all 15 cases with threaded and
non-threaded native Darwin builds. The final source also compiled with
Linux GHC 9.14.1 in both modes, and Cabal planning included the new direct
`process` and `unix` dependencies. The complete Nix proxy package build
reached its 300-second bound after planning. The original Linux proxy
passed minimal `ResolveObjs` and `Shutdown` controls with a 30-second
bound; earlier five-second full-fixture deadlines did not establish a
deadlock. The complete Linux lifecycle suite for the patched proxy is
still pending. These checks do not claim complete CI coverage of the new
proxy package.

The main validation commands were:

```shell
make check test-linux-cross-wrapper
# Exit 0; Nix syntax/selection checks and all three wrapper cases passed.

make verify ISERV_PROXY=/tmp/haskell-nix-iserv-lifecycle-darwin/iserv-proxy-threaded \
  PYTHON=/nix/store/mfkdmplffnbc0av8r6pknl796a6b1r2n-python3-3.13.13/bin/python3
# Exit 0; all 15 threaded lifecycle cases passed.

make test-iserv-lifecycle ISERV_PROXY=/tmp/haskell-nix-iserv-lifecycle-darwin/iserv-proxy-nonthreaded \
  PYTHON=/nix/store/mfkdmplffnbc0av8r6pknl796a6b1r2n-python3-3.13.13/bin/python3
# Exit 0; all 15 non-threaded lifecycle cases passed.

timeout -k 5 180 nix-build --no-out-link /tmp/haskell-nix-qemu-fixture-patched.nix \
  --option max-jobs 1 --option cores 4 --option max-silent-time 90 --option timeout 150
# Exit 0; all four guest cases passed.

timeout -k 5 300 nix-build --no-out-link /tmp/haskell-nix-patched-qemu-repro-instrumented.nix \
  --option max-jobs 1 --option cores 4 --option max-silent-time 90 --option timeout 280
# Inner package build exit-code 0; normal/profiled compilation, install, and fixup passed.
```

These `/tmp` drivers belong to the investigation workspace; the store
artifacts above retain their results. The new CI test registration also
evaluated successfully against the cached unstable nixpkgs snapshot.
Evaluation through the repository's default package import failed because
the local store lacked `/nix/store/kwnjr2jnfn3gsidrw1dkcy3s1v7dc5bw-source.drv`.
The next integration check is the newly evaluated profiled CI job with the
updated overlay and its full package dependencies.

See [the regression test instructions](../../test/README.md) for bounded
host-signal and proxy tests. Increasing `max-silent-time` does not repair
these faults. Native ARM64 QEMU is a verified way to avoid the Rosetta
execution path while investigating or backporting a fix.

For rollout, update the CI source pin and evaluate new derivations before
rerunning the affected profiled job. Retrying build 2014157's immutable
derivation continues to use the old QEMU and proxy. Confirm the new
wrapper's emulator path in the log and run the regression cases on the
physical Rosetta builder; a cached green result is not runtime validation.


[nh2]: https://github.com/nh2/static-haskell-nix
[vaibhav]: https://vaibhavsagar.com/blog/2018/01/03/static-haskell-nix/
[haskell.nix]: https://github.com/input-output-hk/haskell.nix
[bench]: https://hackage.haskell.org/package/bench
[nixpkgs]: https://nixos.org/nixpkgs/manual/#users-guide-to-the-haskell-infrastructure
