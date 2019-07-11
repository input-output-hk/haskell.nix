# Mapping non-Haskell dependencies to Nixpkgs

Cabal files may contain dependencies to external non-Haskell
dependencies via:
* [`build-tool-depends`](https://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-build-tool-depends) 
* [`pkgconfig-depends`](https://cabal.readthedocs.io/en/latest/developing-packages.html#pkg-field-pkgconfig-depends).
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

### Mapping in Haskell.nix

Alternatively, if the name is commonly used, an alias can be added to
the Haskell.nix sources, so that it's solved for all users.

* [`lib/pkgconf-nixpkgs-map.nix`](https://github.com/input-output-hk/haskell.nix/blob/master/lib/pkgconf-nixpkgs-map.nix) - for `pkgconfig-depends`
* [`lib/system-nixpkgs-map.nix`](https://github.com/input-output-hk/haskell.nix/blob/master/lib/system-nixpkgs-map.nix) - for `build-tool-depends`, `frameworks`, `extra-libraries`, etc.
