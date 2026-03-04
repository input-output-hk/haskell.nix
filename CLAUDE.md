# haskell.nix Developer Notes

## General rules

- **Never use materialization** (`materialized` argument).
- **Never use `index-state`** for tools — just pin the version.
- **Never run `cabal` or `ghc --make`** — use `pkgs.haskell-nix.cabalProject` or `pkgs.haskell-nix.tool`
- **Never search /nix/store** — do not use `grep`, `find`, `ls`, or any glob pattern against `/nix/store`. Instead use `nix build` or `nix eval` to find exactly what you are looking for. Searching `/nix/store` is unreliable because it can return stale or unexpected versions from unrelated builds.

## `haskell-nix.tool` — build a tool from Hackage or Source

```nix
pkgs.haskell-nix.tool "ghc9103" "cabal" "3.12.1.0"
pkgs.haskell-nix.tool "ghc9103" "hlint" { version = "3.8"; inherit evalPackages; }
pkgs.haskell-nix.tool "ghc9103" "my-tool" { src = ./path/to/my-tool; }
```

## Using generated source with `haskell-nix.cabalProject`

If possible try to avoid generated source.  If you do have
to generate source avoid large monolythic source derivations.

If the `.cabal` file is not generated use:

```
  modules = [{
    packages.package-name.src = generatedSrc;
  }];
```

If the `.cabal` file is also generated use:

```
  cabalProjectLocal = ''
    packages: ${generatedSrc};
  '';
```

Remove it from `cabal.project` file if necessary.

## Notes

- `"cabal"` is aliased to package `cabal-install`; other aliases in `overlays/tools.nix`.
- Pass `evalPackages` whenever possible.
