# Mapping non-Haskell dependencies to Nixpkgs

Cabal files may contain dependencies to external non-Haskell
dependencies via:

* [`build-tool-depends`](https://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-build-tool-depends)
* [`pkgconfig-depends`](https://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-pkgconfig-depends)
* [`frameworks`](https://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-frameworks)
* [`extra-libraries`](https://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-extra-libraries)

If there is a `pkgs` attribute in Nixpkgs that matches the name given
in the Cabal file, then it will be added as a dependency (see the
output of `cabal-to-nix`). Otherwise, there needs to be a mapping from
Cabal file names (decided by the package author) to Nix package
identifiers.

### Nixpkgs overlay

The user may solve it by themself by overriding Nixpkgs and adding a
package alias. For example:

```nix
nixpkgs.overlays = [
  (self: super: {
    icuuc = self.icu;
    icui18n = self.icu;
    icudata = self.icu;
  })
];
```

### Mapping via project parameter

The user may add arbitrary attributes by adding them to `project'`, `cabalProject'` or `stackProject'`
via the `system-pkgs-overlay` parameter.

For example:

```nix
pkgs.haskell-nix.project' {
  src = ./.;
  compiler-nix-name = "ghcXYZ";
  system-pkgs-overlay = self: super: {
    X11 = super.buildEnv {
      name = "x11-haskell-wrapper";
      paths = with super.xorg; [ libX11 libXScrnSaver libXinerama ]
        ++ [ super.xlibs.libXinerama.dev ];
    };
  };
}
```

### Mapping in Haskell.nix

Alternatively, if the name is commonly used, an alias can be added to
the Haskell.nix sources, so that it's solved for all users.

* [`lib/pkgconf-nixpkgs-map.nix`](https://github.com/input-output-hk/haskell.nix/blob/master/lib/pkgconf-nixpkgs-map.nix)
  — for `pkgconfig-depends`.

  Each mapping entry is a list of packages.

* [`lib/system-nixpkgs-map.nix`](https://github.com/input-output-hk/haskell.nix/blob/master/lib/system-nixpkgs-map.nix)
  — for `build-tool-depends`, `frameworks`, `extra-libraries`, etc.

  Each name can be mapped to:
  1. A single package from nixkpgs.
  2. `null` — eliminates the dependency
  3. A list of packages — sometimes needed for dependencies such as `X11`.

!!! tip "Open a PR"
    Please go ahead and open a [pull request](https://github.com/input-output-hk/haskell.nix/pulls)
    to improve the package mappings.
