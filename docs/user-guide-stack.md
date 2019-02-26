# User Guide (stack project)

Here we will look into how to generate the `pkgs.nix` file for a
`stack.yaml` project.  For the full integration please see the [User
Guide](/user-guide)

## Using `stack-to-nix`

With [nix-tools](https://github.com/input-output-hk/nix-tools) in
`PATH`, we can simply run the following command on a stack project:

```bash
stack-to-nix -o nix --stack-yaml stack.yaml
```

This will produce a `nix/pkgs.nix` file that looks like the following:
```nix
{
  resolver = "lts-12.17";
  overlay = hackage:
    {
      packages = {
        "o-clock" = hackage.o-clock."0.1.1".revisions.default;
        ...
      } // {
        my-package = ./my-package.nix;
        ...
      };
    };
}
```

This file contains the stackage resolver, as well as an overlay of
packages.  The overlay specifies which `extra-deps` (here: o-clock-0.1.1)
we wanted to overlay over the stackage snapshot, and what local
packages we want (here: my-package).

We will then create the following `nix/default.nix` file:

```nix
{ pkgs ? import <nixpkgs> {} }:

let
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz) { inherit pkgs; };

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-overlays = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
```

This generated file is a template, so you can customize it as
necessary.

*If you came here from the [User Guide](/user-guide), go back and
 complete the setup.*
