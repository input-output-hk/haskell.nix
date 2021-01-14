# Handling git repositories in projects

Both `stack.yaml` and `cabal.project` files can contain references
to git repositories containing the version of a particular package
that we wish to use.  This is mostly handled automatically by
`nix-tools` and `haskell.nix` however when we want to use a nix
system that is configured to use restricted mode (typically hydra)
it will need additional hashes for the referenced repositories.

When using `project`, `cabalProject` or `stackProject` functions
you can include the hash needed in a comment.

To calculate the hash use `nix-prefetch-git`:

```
$ nix-prefetch-git https://github.com/input-output-hk/haskell.nix.git bc01ebc05a8105035c9449943046b46c8364b932
...
{
  "url": "https://github.com/input-output-hk/haskell.nix.git",
  "rev": "bc01ebc05a8105035c9449943046b46c8364b932",
  "date": "2019-05-30T13:13:18+08:00",
  "sha256": "003lm3pm024vhbfmii7xcdd9v2rczpflxf7gdl2pyxia7p014i8z",
  "fetchSubmodules": false
}
```

## Cabal.project

Add a `--sha256` comment to the `cabal.project` file:

```
source-repository-package
  type: git
  location: https://github.com/input-output-hk/haskell.nix.git
  tag: bc01ebc05a8105035c9449943046b46c8364b932
  subdir: test/cabal-simple
  --sha256: 003lm3pm024vhbfmii7xcdd9v2rczpflxf7gdl2pyxia7p014i8z
```

## Stack

Add a `# nix-sha256` comment to the `stack.yaml` file:

```
extra-deps:
- git: https://github.com/input-output-hk/haskell.nix.git
  commit: bc01ebc05a8105035c9449943046b46c8364b932
  subdirs:
    - test/cabal-simple
  # nix-sha256: 003lm3pm024vhbfmii7xcdd9v2rczpflxf7gdl2pyxia7p014i8z
```

## Avoiding modifying cabal.project and stack.yaml

In some cases we cannot modify the `cabal.project` or `stack.yaml` file to add
sha256 comments. As an alternative we can pass in a `sha256map`. For instance,
pandoc includes a `cabal.project` file on hackage which includes a
`source-repository-package` stanza for `pandoc-citeproc`:

```nix
{ haskell-nix, testSrc } :
let
  pandoc = haskell-nix.hackage-package {
    name         = "pandoc";
    version      = "2.9.2.1";
    index-state  = "2020-04-15T00:00:00Z"; 
    # Function that returns a sha256 string by looking up the location
    # and tag in a nested attrset
    sha256map =
      { "https://github.com/jgm/pandoc-citeproc"."0.17"
          = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; };
  };
in
  pandoc.components.exes.pandoc
```
