# Reducing closure size (removing the gcc dependency)

By default the executables produced by haskell.nix keep a reference to
the compiler toolchain in their runtime closure.  The most visible
symptom is that a `nix why-depends` on a built executable shows a
dependency on the full `gcc` package (over 100&nbsp;MB), which then ends
up in Docker images and other deployments.

This happens because, unlike the `stdenv` used for C packages in
Nixpkgs, haskell.nix does **not** strip binaries or shrink their RPATHs
by default (see the [rationale](#why-are-these-not-the-default) below).
As a result store-path references to the toolchain — including the
`gcc` used to compile the RTS headers — survive in the output and Nix
records them as runtime dependencies.

This page describes the opt-in knobs you can use to trim those
references.

## Diagnosis

Use `nix why-depends` to confirm what is pulling `gcc` (or any other
unwanted path) into the closure:

```shell
nix why-depends ./result "$(nix-store -q --references ./result | grep gcc)"
```

or, for a flake-based project:

```shell
nix why-depends .#your-exe nixpkgs#gcc
```

The output usually points at a reference embedded in the executable
itself (frequently via the bundled RTS include files), rather than a
genuine shared-library dependency.

## Dynamically linked executables

For a normal, dynamically linked executable, set `dontStrip = false`
(to strip the binary) and `dontPatchELF = false` (to run
`patchelf --shrink-rpath`, Linux only) on the specific `exe` component:

```nix
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "your-project";
    src = ./.;
  };
  compiler-nix-name = "ghc984";

  modules = [{
    packages.your-package.components.exes.your-exe = {
      dontStrip = false;
      dontPatchELF = false;
    };
  }];
}
```

Setting these on the component (rather than globally) keeps the change
scoped to the executable you ship, so libraries used for further
linking are left untouched.

`dontStrip = false` also removes the `--disable-executable-stripping`
and `--disable-library-stripping` flags that haskell.nix otherwise
passes to `Setup`, so Cabal strips as well.

## Fully static executables

For a fully static executable (for example one built against
`pkgsCross.musl64` / `pkgsStatic`), `patchelf --shrink-rpath` has
nothing to shrink and stripping alone may not be enough: absolute
store paths can remain baked into the binary as plain string data,
which Nix still scans and treats as runtime dependencies.

Remove them explicitly with `remove-references-to` (or `nukeReferences`)
in a `postInstall`:

```nix
pkgs.haskell-nix.project {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "your-project";
    src = ./.;
  };
  compiler-nix-name = "ghc984";

  modules = [{
    packages.your-package.components.exes.your-exe = {
      dontStrip = false;
      # Drop references to the compiler and its libraries that are
      # left embedded in the static binary.
      postInstall = ''
        for ref in ${pkgs.buildPackages.gcc} ${pkgs.stdenv.cc.cc.lib}; do
          ${pkgs.buildPackages.removeReferencesTo}/bin/remove-references-to \
            -t "$ref" "$out/bin/your-exe"
        done
      '';
    };
  }];
}
```

Add each store path that `nix why-depends` reports as unwanted to the
loop.  `nukeReferences` (`${pkgs.buildPackages.nukeReferences}/bin/nuke-refs`)
is a blunter alternative that rewrites *every* store-path reference in
the given files; only use it when you are certain the binary has no
legitimate runtime dependencies.

## Why are these not the default?

Stripping is not enabled by default because it has historically been
reported to occasionally break Haskell binaries — the stripper can be
too aggressive given the way Haskell code is laid out in object files
(see the discussion in
[haskell.nix#829](https://github.com/input-output-hk/haskell.nix/issues/829)
and [haskell.nix#336](https://github.com/input-output-hk/haskell.nix/pull/336#discussion_r351491851)).
Whether this is still a problem with current GHC and binutils has not
been conclusively established, so the safe choice is to leave stripping
off by default and let you opt in per component.

Because of that history, **test the affected executables after enabling
these options** — run them, run their test suites, and check anything
that relies on runtime code loading (Template Haskell is a build-time
concern and is unaffected, but plugin-style `dlopen` of Haskell objects
is worth checking).

See `dontStrip` and `dontPatchELF` in the
[Module options](../reference/modules.md) reference.
