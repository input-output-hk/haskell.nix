This file contains a summary of changes to Haskell.nix and `nix-tools`
that will impact users.

## Jan 29, 2025

Removed GHC <9.6 from CI.

The latest `nixpkgs-unstable` caused problems with
  * GHC 8.10.7
  * GHC 9.6.6 mingwW64 (ucrt64 works still as does mingwW64
    with newer GHC versions)

## Sep 17, 2024

Cabal projects now use the more granular Unit IDs from plan.json
to identify packages.  This allows for different versions of a
package to be used when building `built-tool-depends` and setup
dependencies.

Overrides in the `modules` argument apply to all versions of
the package.  However to make this work we needed to make
each `packages.somepackage` an option (instead of using an
`attrsOf` the submodule type).

It is now an error to override a package that is not in the
plan.  This can be a problem if different GHC versions, target
platforms, or cabal flag settings cause the package to be
excluded from the plan.  Adding `package-keys` can tell
haskell.nix to include the option anyway:

```
  modules = [{
    # Tell haskell.nix that `somepackage` may exist.
    package-keys = ["somepackage"];
    # Now the following will not cause an error even
    # if `somepackage` is not in the plan
    packages.somepackage.flags.someflag = true;
  }];
```

There is a helper function you can use to add `package-keys`
for all of the `builtins.attrNames` of `packages`:

```
  modules = [(pkgs.haskell-nix.haskellLib.addPackageKeys {
    packages.somepackage.flags.someflag = true;
  })];
```

Do not use the module's `pkgs` arg to look `addPackageKeys` up
though or it will result an `infinite recursion` error.

Code that uses `options.packages` will also need to be updated.
For instance the following code that uses `options.packages`
to set `--Werror` for local packages:

```
  ({ lib, ... }: {
    options.packages = lib.mkOption {
      type = lib.types.attrsOf (lib.types.submodule (
        { config, lib, ... }:
        lib.mkIf config.package.isLocal
        {
          configureFlags = [ "--ghc-option=-Werror"];
        }
      ));
    };
  })
```

Now needs to do it for each of the entry in `config.package-keys`
instead of using `attrsOf`:

```
  ({ config, lib, ... }: {
    options.packages = lib.genAttrs config.package-keys (_:
      lib.mkOption {
        type = lib.types.submodule (
          { config, lib, ... }:
          lib.mkIf config.package.isLocal
          {
            configureFlags = [ "--ghc-option=-Werror"];
          }
        );
      });
  })
```

## Jun 5, 2024

Haskell.nix now respects the `pre-existing` packages selected
by the cabal planner.  The selection made by the planner
is used to set `nonReinstallablePkgs`.

Instead setting `nonReinstallablePkgs` and `reinstallableLibGhc`
haskell.nix projects should add `constraints` to the cabal project.

For instance to force the use of the `pre-exising` `text`
package add:

```
  constraints: text installed
```

To make sure `text` is reinstalled use:

```
  constraints: text source
```

The `pre-existing` `ghc` will now be used by default as
that is what `cabal` will choose (haskell.nix used to choose
`reinstallableLibGhc=true` by default).

To allow cabal to choose reinstalling `ghc` add:

```
  allow-boot-library-installs: True
```

To force cabal to choose reinstalling:

```
  constraints: ghc source
  allow-boot-library-installs: True
```

It may also need `allow-newer: ghc:Cabal`

## Mar 27, 2023

Haskell.nix will no longer parse the `cabal.project` file to
determine the `index-state`. This decision was made due to
the function's inability to handle more than one `index-state`
or a qualified `index-state` as the first `index-state`
field in the file.

As a result, there will be some drawbacks:

* There will no longer be a warning in the trace output
  if an index state is not found.

* Even if the `index-state:` in the `cabal.project` has not changed,
  the plan will be recomputed when hackage.nix is bumped. However, this
  is not expected to be a problem since plan recomputations are typically
  quick.

* `project.index-state` cannot be used to obtain the found `index-state`.
  However, the parse function is still available if required
  (haskell-nix.haskellLib.parseIndexState).  

## Jul 27, 2022
* Removed reliance on `builtins.currentSystem`.  It was used it to provide
  `pkgs.evalPackages` via an overlay that it used to run derivations
  used in imports from derivation (IFDs).

  These derivations are now run on `buildPackages` by default.

  Passsing `evalPackages` to a project function will change where all the
  derivations used in IFDs are run for that project (including shell tools):
    evalPackages = import nixpkgs haskellNix.nixpkgsArgs;

  Passing `evalSystem` instead will use create a suitable `nixpkgs` using `pkgs.path`
  and `pkgs.overlay`:
    evalSystem = "x86_64-linux";
  or
    evalSystem = builtins.currentSystem;

  The `haskellLib.cleanGit` function is also affected by this change.  If you are cross
  compiling and using `cleanGit` you should probably do something like:
    pkgs = import nixpkgs haskellNix.nixpkgsArgs;
    evalPackages = import nixpkgs (haskellNix.nixpkgsArgs // { system = evalSystem; });
    p = pkgs.pkgsCross.mingwW64.haskell-nix.cabalProject {
      inherit evalPackages;
      src = evalPackages.haskell-nix.haskellLib.cleanGit { src = ./.; };
    };

## Feb 16, 2022
* Removed lookupSha256 argument from project functions.
  Pass a `sha256map` instead.
* Added better support for `repository` in `cabal.project`.  These
  blocks should now work without the need for passing `extra-hackages` and
  `extra-hackage-tarballs`.

## Aug 6, 2021
* Included dependencies of haskell.nix that were tracked in `nix/sources.json`
  as flake inputs (`flake.lock` replaces `nix/sources.json`).
* Uses `flake-compat` to continue to provide a compatible interface for non
  flake projects.

## Jul 23, 2021
* `source-repository-package` references in `cabal.project` files are now
  left as a `source-repository-package` when calculating the the `plan-nix` for
  `cabalProject` based functions.
  This makes haskell.nix match the behaviour of `cabal` better.
  Materialized files for projects that use `source-repository-package`
  references will need to be updated.
* Only planned components are included in a haskell.nix cabal project.
  If cabal solver does not include the component in the `plan.json` file it
  will not be present in `hsPkgs.pkg.components`.
* When the same package occurs more than once in a plan.json file
  the latest version is picked by haskell.nix.

## Apr 8, 2021
* Project arguments are now validated with the Nix module system.
  If unexpected argments are passed to a project function this may now
  result in an error.

## Feb 22, 2021
* Add `.dwarf` to build any component with DWARF dubug info on linux
  (ghc >=8.10.2).
* Pass `enableDWARF` to `shellFor` for to get a shell where all the
  components are the `.dwarf` ones.

## Feb 18, 2021
* `ghcOptions` has been moved from package and is now a list of strings.
    old: packages.x.package.ghcOptions = "someGHCoption";
    new: packages.x.ghcOptions = ["someGHCoption"];
  To specify ghcOptions for all packages:
    ghcOptions = ["someGHCoption"];
  For a single component:
    packages.x.compoents.library.ghcOptions = ["someGHCoption"];

## Feb 8, 2021
* Removed older versions of haskell-language-server from custom-tools
  (0.8.0 is in hackage so we can still get that version).

## Jan 14, 2021
* Added support for cross package refs (with a project).  Relative
  directory references between packages within a project should now
  work.
* Added `includeSiblings` to `cleanSourceWith`.  When `true` it
  prevents the `subDir` arg from causing filtering of other directories.
* Added `keepGitDir` to `cleanGit` to allow `.git` directory to be kept
  (useful for components that use the `githash` package).

## Nov 26, 2020
* Renamed `otherShells` arg for `shellFor` to `inputsFrom

## Nov 25, 2020
* The `shellFor` `makeConfigFiles` `ghcWithHoogle` and `ghcWithPackages`
  functions have been removed from `project.hsPkgs`.  Instead access
  them from `project` itself (e.g. change `p.hsPkgs.shellFor` to `p.shellFor`).
* The reflex-platform like `project.shells.ghc` has been removed.
  If needed, add something like `p // { shells.ghc = p.shellFor {} }`
  to `shell.nix`.

## Nov 24, 2020
* Added `${targetPrefix}cabal` wrapper script for running cross
  compilers in `shellFor`.
* `otherShells` arg added to `shellFor`.

## Oct 31, 2020
* Passing `tools.hoogle` to `shellFor` with a value suitable for `haskel-nix.tool` will
  use the specified `hoogle` inside `shellFor`. This allows for materialization
  of `hoogle`.

## Oct 28, 2020
* Passing `compiler-nix-name` to project functions for `stack.yaml`
  based projects now overrides the compiler used (was ignored before).

## Sep 8, 2020
* Added the ability to generate coverage reports for packages and
  projects.
* Added the `doCoverage` module option that allows users to choose
  packages to enable coverage for.
* Added a `doCoverage` flag to the component builder that outputs HPC
  information when coverage is enabled.
* Added test for coverage.

## July 21, 2020
* Removed `components.all`, use `symlinkJoin` on components.exes or
 `shellFor` if you need a shell.
* Added `components` argument to `shellFor`.

## July 21, 2020
* Added GHC 8.8.4 and replaced 8.8.3 in tests and as the ghc
  used to build nix-tools for stack projects.

## July 20, 2020
* Changed `haskell-nix.roots` and `p.roots` to single derivations.

## July 8, 2020
* Removed `sources.nixpkgs-default`, use `sources.nixpkgs` instead.
* Removed `./nixpkgs` directory, use  `(import ./. {}).sources`
  or `./nix/sources.nix` instead.
* Removes V1 interface for details on how to fix old code see:
    https://github.com/input-output-hk/haskell.nix/issues/709
* Removed defaultCompilerNixName.
* cabalProject, cabalProject', hackage-project and hackage-package
  now require a `compiler-nix-name` argument.
* `haskell-nix.tool` and `.tools` now require a `compiler-nix-name` argument.
  New functions `p.tool` and `p.tools` (where p is a project) do not.
  Like `shellFor { tools = ... }` they will use the compiler nix name
  from the project (including stack projects where it is derived from
  the resolver).
* `haskell-nix.alex` and `haskell-nix.happy` have been removed. Use
  `p.tool "alex" "3.2.5"` or `shellFor { tools = { alex = "3.2.5"; } }`.
* `haskell-nix.nix-tools` -> `haskell-nix.nix-tools.ghc883` (it includes
  the hpack exe now).
* `haskell-nix.cabal-install` ->
  `p.tool "cabal" "3.2.0.0"` or `shellFor { tools = { cabal = "3.2.0.0"; } }`
* `haskell-nix.haskellNixRoots` -> `haskell-nix.roots ghc883` or `p.roots`

## June 25, 2020
* Haddock docs are now built in their own derivation when needed (not as part
  of the component build).
  They should build automatically when something (such as `shellFor`) attempts
  to accesses the `.doc` attribute of component.

## December 27, 2019
* Fix overlays/bootstrap.nix to provide LLVM 6, not LLVM 5, to ghc-8.6.X compilers.

## November 18, 2019
  * Changed the `cleanSourceHaskell` to accept an attrset of `src` and
    (optional) `name` parameters. This allows you to keep the source
    derivation name constant, so that your builds are always
    cached. Usage of `cleanSourceHaskell` will need to be updated.

## October 12, 2019
 * [`shellFor`](https://input-output-hk.github.io/haskell.nix/reference/library/#shellfor) no longer sets `CABAL_CONFIG` by default.
   This avoids surprising users, but means that Cabal may select a plan which is different to your Haskell.nix package set.
   If you would like the old behaviour, use `shellFor { exactDeps = true; }`.

## August 9, 2019
 * Add the [`haskellLib.collectComponents`](https://input-output-hk.github.io/haskell.nix/reference/library/#haskellLib) function.

## June 21, 2019
 * Add `ghcWithPackages` and `ghcWithHoogle` to hsPkgs ([documentation](https://input-output-hk.github.io/haskell.nix/reference/library/#package-set-functions).
 * Benchmark components can now build successfully.
 * Reduced the closure bloat of nix-tools, and added closure size limit to CI.
 * Added more reference documentation and set up auto-generated
   documentation for [Module Options](https://input-output-hk.github.io/haskell.nix/reference/modules/).
 * Miscellaneous bug fixes.

## June 7, 2019
  * Several additions to the [documentation](https://input-output-hk.github.io/haskell.nix/).
    * More information about getting nix-tools, Haskell.nix, pinning.
    * Updates the stack-to-nix and cabal-to-nix guides.
    * Adds a section on development environments.
    * Adds a little information about cross compilation.
    * Adds a (partially complete) reference section (command line manuals, library reference).
    * Symlinks the changelog into the documentation pages.

## May 29, 2019
  * Added `shellFor` function to package set.

## May 28, 2019
  * Added `snaphots` and `haskellPackages` attributes to the
    Haskell.nix top-level.

## May 22, 2019
  * Add the `cleanSourceHaskell` utility function to the Haskell.nix
    top-level.

## May 21, 2019
  * Add the `callCabalProjectToNix` function, which uses "import from
    derivation" (IFD) so that nix-tools doesn't need to be run
    manually.
  * The `hackage.nix` update process has changed, so that Cabal index
    state hashes are also included in the generated repo.

## May 20, 2019
  * Remove Travis CI in favour of Buildkite.

## May 17, 2019
  * Add the `callStackToNix` function, which uses "import from
    derivation" (IFD) so that `stack-to-nix` doesn't need to be run
    manually.

## Mar 15, 2019
  * `overlays` was renamed to `extras` in
    [#79](https://github.com/input-output-hk/haskell.nix/pull/79)
    to prevent confusion between the notion of Nix overlays.

    Therefore `plan-pkgs` and `stack-pkgs` as generated by `plan-to-nix` and `stack-to-nix` will
    expose `extras` instead of `overlay`. Similarly `mkStackPkgSet`, `mkPkgSet` and `mkCabalProjectPkgSet`
    take a `pkg-def-extras` instead of `pkg-def-overlay` argument.  If you are using `iohk-nix`, the
    `iohk-overlay` was parameter was renamed to `iohk-extras`.
