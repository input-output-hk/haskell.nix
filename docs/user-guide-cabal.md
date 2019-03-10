# User Guide (cabal project)

Here we will look into how to generate the `pkgs.nix` file for a
`cabal.project` project.  For the full integration please see the [User
Guide](/user-guide)

## Using `plan-to-nix`

*We currently don't have a `project-to-nix` tool yet, as such creating
 the relevant `pkgs.nix` file for a `cabal.project` is slightly more
 involved than for a corresponding stack project*.

With [nix-tools](https://github.com/input-output-hk/nix-tools) in
`PATH`, we can simply run the following command on a cabal package:

```bash
# make sure the cabal project is configured (the plan.json file is generated)
cabal new-configure
# convert the plan.json file into a pkgs.nix file
plan-to-nix dist-newstyle/cache/plan.json > nix/plan.nix
```

This will produce a `nix/plan.nix` file that looks like the following:
```nix
hackage:
  {
    packages = {
      "o-clock" = hackage.o-clock."0.1.1".revisions.default;
      ...
    };
    compiler = { ... };
  }
```

it specifically does not include any of our local packages yet. We
will need to run

```bash
cabal-to-nix $path > nix/$pkg.nix
```
or
```bash
cabal-to-nix $url $rev > nix/$pkg.nix
```
for each local (or source) package.

With this in place we can then proceed to build the `nix/pkgs.nix`
file as follows:

```nix
let plan = import ./plan.nix; in
{ pkg-def = plan;
  overlay =
    { local-package-a = ./local-package-a.nix;
      local-package-b = ./local-package-b.nix;
      source-import-a = ./source-import-a.nix;
      source-import-b = ./source-import-b.nix;
      ...
    };
}
```

*If you came here from the [User Guide](/user-guide), go back and
 complete the setup.*
