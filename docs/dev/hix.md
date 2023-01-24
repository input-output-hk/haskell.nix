# Making changes to Hix

When making changes to the way Hix works it is often useful to be able to
test the changes locally before uploading them to github.

## Hix Command Wrappers

Install the hix command wrappers after making changes to a local clone of haskell.nix:

```shell
nix-env -iA hix -f /path/to/local/haskell.nix
hix-shell
```

Or override the version of haskell.nix used by the commands with the `HIX_ROOT` environment variable:

```shell
HIX_ROOT=/path/to/local/haskell.nix hix-shell
```

## Flakes

For flakes use `--override-input` to point to the modified haskell.nix:

```shell
nix develop --override-input haskellNix /path/to/local/haskell.nix
```