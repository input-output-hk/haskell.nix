# IOHK's nix tooling

## [`iohk-nix`](https://github.com/input-output-hk/iohk-nix)

`iohk-nix` is IOHK's shared nix library. It provides some templates to
make working with `haskell.nix` trivial but is non-essential to use
`haskell.nix` infrastructure.

### `lib.nix`

```nix
{{#include iohk-nix/lib.nix}}
```

### `iohk-nix.json`
```json
{{#include iohk-nix/iohk-nix.json}}
```

### `nix/pkgs.nix`

```nix
{{#include iohk-nix/nix/pkgs.nix}}
```

### `default.nix`

```nix
{{#include iohk-nix/default.nix}}
```
