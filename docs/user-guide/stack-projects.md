Here we will look into how to generate the `pkgs.nix` file for a
`stack.yaml` project. For the full integration see the
[previous page](./projects.md).

## Using `stack-to-nix`

With [nix-tools installed](../user-guide.md), we can simply run the
following command on a stack project:

```bash
stack-to-nix --output . --stack-yaml stack.yaml
```

This will produce a `pkgs.nix` file that looks like the following:
```nix
{
  resolver = "lts-12.17";
  extras = hackage:
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

This file contains the stackage resolver, as well as set of extra
packages.  The extras specifies which `extra-deps` (here: o-clock-0.1.1)
we wanted to add over the stackage snapshot, and what local
packages we want (here: `my-package`).
