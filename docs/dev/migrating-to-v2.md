# Migrating a project from `builderVersion = 1` to `builderVersion = 2`

The v2 builder runs `cabal v2-build` inside each per-component slice
derivation rather than driving `Setup.hs` from haskell.nix.  As a
consequence, configuration that haskell.nix used to consume *after*
plan-to-nix (via module-level options) now has to be visible to cabal
*at* plan-to-nix time — either in `cabal.project` /
`cabalProjectLocal`, or routed via mechanisms that feed plan.json's
`configure-args`.

This document collects the patterns we hit migrating the in-tree test
suite.  If you're flipping `builderVersion = 2` on your own project
and see one of the symptoms below, follow the matching pattern.

## Routing settings through `cabalProjectLocal`

Several haskell.nix module options that v1 honoured directly are no
longer enough on their own.  v2 reads plan.json's per-unit
`configure-args` to compute UnitIds, and anything not visible in
cabal.project at plan time forks the slice's UnitId hash from
plan-nix's recorded UnitId — symptoms include "unit … not found" at
slice link time, or quietly re-resolved versions.

### Library / executable profiling

**Old (v1):**

```nix
modules = [{
  packages.foo.components.library.enableLibraryProfiling = true;
  packages.foo.components.exes.bar.enableProfiling       = true;
}];
# ... then later:
project.hsPkgs.foo.components.exes.bar.profiled
```

**New (v2):**

```nix
cabalProjectLocal = ''
  package *
    library-profiling: True
  package foo
    profiling: True
'';
# ... then:
project.hsPkgs.foo.components.exes.bar
```

The `.profiled` overlay attribute is no longer produced by the v2
builder.  To build a profiled variant alongside a non-profiled one,
instantiate **a second sibling project** whose `cabalProjectLocal`
carries the profiling toggles — see `test/exe-dlls/default.nix`,
`test/th-dlls/default.nix`, and `test/gi-gtk/default.nix` for
patterns that thread a `profiled` argument through the project
constructor.

### Coverage

**Old:**

```nix
modules = [{
  packages.pkga.doCoverage = true;
  packages.pkgb.doCoverage = true;
}];
```

**New:** keep the module entries (still honoured), but also mirror
them in `cabalProjectLocal`:

```nix
cabalProjectLocal = ''
  package pkga
    coverage: True
  package pkgb
    coverage: True
'';
```

`test/coverage/default.nix` shows the pattern.

### DWARF debug info

**Old (v1):**

```nix
project.hsPkgs.foo.components.exes.bar.dwarf
```

**New (v2):**

```nix
project = cabalProject' {
  compilerSelection = p: lib.mapAttrs (_: c: c.dwarf) p.haskell-nix.compiler;
  cabalProjectLocal = ''
    package *
      debug-info: 2
  '';
  # …
};
# ... then:
project.hsPkgs.foo.components.exes.bar
```

Reasons:

* The `.dwarf` slice overlay would diverge from plan-nix's UnitId
  (slice cabal sees different `-g` flags than plan-nix's dummy GHC),
  so it's dropped — `comp-v2-builder` throws a hint pointing here if
  you reference `.dwarf`.
* `compilerSelection` routes the swap through the *whole* compiler
  chain (plan-nix, slice GHC, dummy-ghc), so DWARF covers both your
  code and the runtime — matching what `<slice>.dwarf` used to do
  for runtime coverage.

`test/cabal-simple-debug/default.nix` shows the pattern.

### Haddock documentation

```nix
cabalProjectLocal = ''
  package *
    documentation: True
'';
```

Without this, v2 doesn't produce sublib haddocks.  `test/sublib-docs`
shows the pattern.  Note the on-disk layout also changed: v1 writes
`share/doc/<pkg>/html/<Module>.html`, v2 writes
`share/doc/html/<Module>.html` (the unit-id is hashed into the path,
so don't synthesise paths — use `find -name '<Module>.html'`).

### Per-package `ghc-options`

**Old (v1):**

```nix
modules = [{
  packages.HsOpenSSL.ghcOptions = ["-optc=-Wno-incompatible-pointer-types"];
}];
```

**New (v2):** move into `cabal.project.local`:

```
package HsOpenSSL
  ghc-options: -optc=-Wno-incompatible-pointer-types
```

`test/cabal.project.local` and `test/modules.nix` show the
migration — the `package-keys` entry stays in `modules.nix` only if
*other* per-package overrides need it.

### Per-package `extra-lib-dirs` / `extra-include-dirs`

**Old (v1):**

```nix
modules = [{
  packages.test-lib.components.library.extraLibDirs = ["${test-clib}/lib"];
}];
```

**New (v2):** prefer `cabal.project`:

```nix
cabalProjectLocal = ''
  package test-lib
    extra-lib-dirs: ${test-clib}/lib
    extra-lib-dirs: ${test-clib}/bin
'';
```

`test/th-dlls-minimal/default.nix` shows the Windows case.

## Module-level option scope changes

A few options moved from per-component to per-package scope.

### `ghcOptions`

**Old (v1):**

```nix
modules = [{
  packages.th-dlls.components.library.ghcOptions = [ "-fexternal-interpreter" ];
}];
```

**New (v2):**

```nix
modules = [{
  packages.th-dlls.ghcOptions = [ "-fexternal-interpreter" ];
}];
```

v2 applies the package-level setting to all components in the slice.

### Module options on packages outside the local project

If your `modules` set options on a hackage package that isn't a
"home" package (e.g. `HsOpenSSL`), declare the key:

```nix
modules = [{
  package-keys = [ "HsOpenSSL" ];
  packages.HsOpenSSL.ghcOptions = [ … ];
}];
```

Without this, v2's per-package option lookup misses the override.

## `shellFor` instead of `component.shell` / `component.env`

v2 doesn't produce per-component `.shell` / `.env` attributes.  Use
the project-level shell:

**Old (v1):**

```nix
test-shell = project.hsPkgs.test-with-packages.components.library.shell;
```

**New (v2):**

```nix
test-shell = project.shellFor {
  packages = ps: [ ps.test-with-packages ];
  exposePackagesVia = "ghc-pkg";
};
```

`exposePackagesVia` controls how the dep closure is exposed to the
shell:

* `"cabal-store"` (default) — seeds `~/.cabal/store` from the
  composed slice store so plain `cabal v2-build` in the shell
  reuses the haskell.nix-built units.
* `"ghc-pkg"` — wraps `ghc` and `ghc-pkg` with
  `GHC_ENVIRONMENT` / `GHC_PACKAGE_PATH` so `ghc -e …` /
  `ghc-pkg list` see the dep closure directly.  Use this when you
  want runghc-style invocations to find the deps without going
  through cabal.

`test/with-packages/default.nix` and `test/shell-for/default.nix`
show the migration.

### `withHoogle = false` on `static`

On musl-static the GHC compiler tree ships no `.dyn_hi` files
(only `.hi` and `.p_hi`).  Hoogle's haddock chain tries to
haddock-build packages whose TH needs `.dyn_hi`
(e.g. `OneTuple` reaching `Language.Haskell.TH.dyn_hi`) and fails:

```
Exception when reading interface file
  …/template-haskell-2.24.0.0/Language/Haskell/TH.dyn_hi:
  does not exist (No such file or directory)
```

Mitigation: gate `withHoogle` off when static.

```nix
shellArgs.withHoogle = !stdenv.hostPlatform.isStatic;
```

## Source-tree overrides

### `origSrc` / `origSubDir` no longer plumbs `.git` into the build

v1's `comp-builder` copied the whole `origSrc` into the build env
and `cd`d to `origSubDir`, so TH that walked up the tree for `.git`
(`githash`'s `tGitInfoCwd`) found it.  v2's slice tarballs only the
package subdir, so the parent `.git` is lost.

Workaround: synthesise a *package root* that has `.git` at its top
and pass it as `src`:

```nix
packageRoot = evalPackages.runCommand "my-pkg-root" {} ''
  mkdir -p $out/.git
  cp -rL ${packageSrc}/. $out/
  cp -r ${gitDir}/HEAD    $out/.git/HEAD
  cp -r ${gitDir}/refs    $out/.git/refs
  cp -r ${gitDir}/objects $out/.git/objects
'';
project = cabalProject' {
  src = { outPath = packageRoot; filterPath = { path, ... }: path; };
  # …
};
```

The `outPath + filterPath` form tells haskell.nix the source is
already filtered — without this the default `haskellSourceFilter`
strips `.git`.  `test/githash/default.nix` shows the full pattern.

## Setup dependencies on custom-setup packages

cabal v2-build doesn't auto-add boot packages to `setup-depends`.
If your custom Setup.hs imports from `base` or `directory`, list
them explicitly:

```cabal
custom-setup
  setup-depends:
      base
    , directory
    , <your other deps>
```

`test/setup-deps/pkg/pkg.cabal` and
`test/shell-for-setup-deps/pkg/pkg.cabal` show the change.

## Platform-specific gotchas

### Native-musl shell tools

The v2 slice ghc wrapper on native-musl sets
`LD_LIBRARY_PATH=${musl-gcc-libs}` so `iserv-dyn` can find
`libgcc_s` at TH-eval.  Glibc-linked subprocesses cabal spawns
(notably `git` for `source-repository-package`) would then load musl
libc and segfault.  When picking which `git` to put in
`build-tools`, use the *host* platform's git on native-musl (a musl
binary that already runs on the build machine), and the
*build-build* git everywhere else:

```nix
git =
  if haskellLib.isNativeMusl
    then gitReallyMinimal
    else buildPackages.buildPackages.gitReallyMinimal;
```

`test/githash/default.nix` shows the pattern.

### Android cross builds

Android cross hosts need a few `cabal.project` directives that
neither plan-to-nix nor cabal infer from the cross GHC's `--info`.
The in-tree `test/cabal.project.android` carries them; project tests
that exercise android targets inject it conditionally:

```nix
cabalProjectLocal = builtins.readFile ../cabal.project.local
  + lib.optionalString stdenv.hostPlatform.isAndroid
      (builtins.readFile ../cabal.project.android);
```

### `useLocalGhcLib = true` for `lib:ghc` consumers

If your project depends on or constrains the `ghc` package
(e.g. `ghc-lib-reinstallable`), set `useLocalGhcLib = true` on the
project.  See the entry in `changelog.md`
(2026-05-23) and `test/ghc-lib-reinstallable/cabal.nix`.

## Suggested migration order

1. **Flip `builderVersion = 2`** on a non-critical branch and let CI
   tell you which tests / components fail.
2. **Profiling and coverage first** — they're the most mechanical
   and cover the largest blast radius.  Move toggles into
   `cabalProjectLocal` and introduce sibling projects for the
   `.profiled` callers.
3. **DWARF / debug-info** if you have any consumers of `.dwarf`.
4. **Shell migration** (`component.shell` / `component.env` →
   `project.shellFor`).
5. **Per-package `ghcOptions` / `extraLibDirs`** in `modules` —
   move to `cabal.project` where possible; declare `package-keys`
   where the package isn't in the home project.
6. **`origSrc` users** (rare — typically `githash`-style TH that
   walks up to `.git`): synthesise a package-root drv.
7. **Custom-setup `base` / `directory`** — add explicit
   `setup-depends`.

If a slice fails to link with `unit … not found`, the cause is
almost always plan-nix and slice cabal disagreeing on a UnitId
input — re-check that every option that affects the UnitId hash
(profiling, debug-info, library-for-ghci, ghc-options,
extra-lib-dirs, extra-include-dirs, …) is visible in
`cabal.project` rather than only in `modules`.
