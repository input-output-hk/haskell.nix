# Building with profiling

Under the **v1** builder, every component derivation exposed a
`.profiled` attribute that re-built that component with
`enableLibraryProfiling = true`.  Callers — including
`tests.exe-dlls.check-profiled` and any user code that does
`pkg.components.exes.foo.profiled` — relied on this overlay-style
rebuild.

Under the **v2** builder this overlay-rebuild path no longer exists.
v2 reads its configure-args from `plan.json`, and `plan.json` doesn't
know the haskell.nix module-level `enableLibraryProfiling` /
`enableProfiling` flags.  Producing a profiled variant by overriding
those flags after the fact would emit `--enable-profiling` /
`--enable-library-profiling` Setup configure args that aren't in
plan-nix, the slice's UnitId would diverge from plan-nix's recorded
UnitId, and the slice would fail its UnitId check.

## How to build profiled binaries with v2

Move the configure-time toggles to `cabal.project` (or the test's
`cabalProjectLocal` argument) so plan-nix records them.  Project-wide:

```cabal
package *
  library-profiling: True
```

Per-package:

```cabal
package my-exe-pkg
  profiling: True            -- enable executable profiling
  library-profiling: True    -- (optional, for the package's own lib)
```

The slice's UnitId will then match plan-nix and the produced binary
is a real `-prof` build — plain `nix build .#…components.exes.foo`
gives you a profiled exe; no separate `.profiled` attribute needed.

## Why not just rebuild?

A reproducible `.profiled` overlay would require re-solving plan-nix
with the profiling toggles applied (so the recorded UnitIds are the
profiled ones), or skipping the UnitId check entirely (giving up the
guarantee that the slice's outputs match plan-nix).  Neither belongs
in the per-component slice path; the right place to express
"profiled build" is where every other UnitId-affecting toggle lives —
in `cabal.project`.
