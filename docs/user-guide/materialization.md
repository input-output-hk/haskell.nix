# Materialization

## What is materialization?

Capturing and storing the nix files for a project so that they do
not need to be built (or checked).  This allows us to cache the input
of an IFD (import from derviation).

## Why use materialization?

Using functions like `cabalProject`, `stackProject` and `hackage-package`
results in a lot of dependencies (all the dependencies of nix-tools
for instance).

* They can be slow to calculate (even if no work needs to be done it
  is not unusual for it to take 5 seconds per project).

* They can be slow to build (or download) on machines that do not
  yet have them in the nix store.

* Hydra does not show progress because it does not provide feedback
  until it has a list of jobs and the list of jobs cannot depends
  on the nix being present (although this is often blamed on IFD
  it would be the same if it wrote out JSON files and read them in)

## When is it ok to materialize?

* The nix is unlikely to change frequently (and when it does you
  are happy to manually update it).

* You are happy to script something to update the materialized
  nix files automatically.
* You are certain that the IFD you materialize is not `system`-dependent. If it was you'd
   obtain different nix expressions depending on which `system` the IFD was evaluated.
   
## How can we materialize the nix files?

Lets say we want to build `hlint`.  We might start with an `hlint`
file that looks like this:

```nix
let inherit (import ./. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
    hlint = pkgs.haskell-nix.hackage-package {
      name = "hlint";
      version = "2.2.11";
    };
in hlint.components.exes.hlint
```

Building this may result in a lot of output, but if you build
it again it should give just:

```
$ nix-build hlint.nix
trace: Using latest index state for hlint!
trace: Using index-state: 2020-04-15T00:00:00Z for hlint
/nix/store/rnfz66v7k8i38c8rsmchzsyqjrmrbdpk-hlint-2.2.11-exe-hlint
```

To materialize the nix files we need to take care to pin down the
inputs.  For `cabalProject` and `hackage-package` this means
we must specify the `index-state` of hackage we want to use:

```nix
let inherit (import ./. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
    hlint = pkgs.haskell-nix.hackage-package {
      name = "hlint";
      version = "2.2.11";
      index-state = "2020-04-15T00:00:00Z";
    };
in hlint.components.exes.hlint
```

Now if we build again we get a hint telling use how to
calculate a suitable sha256 hash to turn the derivation
containing the nix files into a fixed output derivation:

```
$ nix-build hlint.nix
trace: Using index-state: 2020-04-15T00:00:00Z for hlint
trace: Get `plan-sha256` with `nix-hash --base32 --type sha256 /nix/store/8z6p4237rin3c6c1lmjwshmj8rdqrhw2-hlint-plan-to-nix-pkgs/`
/nix/store/rnfz66v7k8i38c8rsmchzsyqjrmrbdpk-hlint-2.2.11-exe-hlint

$ nix-hash --base32 --type sha256 /nix/store/8z6p4237rin3c6c1lmjwshmj8rdqrhw2-hlint-plan-to-nix-pkgs/
02hasr27a994sml1fzf8swb716lm6lgixxr53y0gxkhw437xkck4
```

We can add the hash as `plan-sha256` or (`stack-sha256` for
`stackProject`)

```nix
let inherit (import ./. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
    hlint = pkgs.haskell-nix.hackage-package {
      name = "hlint";
      version = "2.2.11";
      index-state = "2020-04-15T00:00:00Z";
      plan-sha256 = "02hasr27a994sml1fzf8swb716lm6lgixxr53y0gxkhw437xkck4";
    };
in hlint.components.exes.hlint
```

Just adding the hash might help reuse of the cached nix, but nix will
still calculate all the dependencies (which can add seconds to
`nix-build` and `nix-shell` commands when no other work is needed)
and users who do not yet have the dependencies in their store will have
to wait while they are built or downloaded.

Running nix build again gives us a hint on what we can do next:

```
$ nix-build hlint.nix
trace: Using index-state: 2020-04-15T00:00:00Z for hlint
trace: To materialize, point `materialized` to a copy of /nix/store/kk047cqsjvbj4w8psv4l05abdcnyrqdc-hlint-plan-to-nix-pkgs
/nix/store/rnfz66v7k8i38c8rsmchzsyqjrmrbdpk-hlint-2.2.11-exe-hlint
```

To capture the nix we can do something like:

```nix
let inherit (import ./. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
    hlint = pkgs.haskell-nix.hackage-package {
      name = "hlint";
      version = "2.2.11";
      index-state = "2020-04-15T00:00:00Z";
      plan-sha256 = "02hasr27a994sml1fzf8swb716lm6lgixxr53y0gxkhw437xkck4";
      materialized = ./hlint.materialized;
    };
in hlint.components.exes.hlint
```

Now we can copy the nix files needed and build with:

```
$ cp -r /nix/store/8z6p4237rin3c6c1lmjwshmj8rdqrhw2-hlint-plan-to-nix-pkgs hlint.materialized
$ nix-build hlint.nix
/nix/store/rnfz66v7k8i38c8rsmchzsyqjrmrbdpk-hlint-2.2.11-exe-hlint
```

We may want to run `chmod -R +w hlint.materialized` as the files copied from the
store will be read only.

## How can we check `sha256` and `materialized` are up to date?

Let's pretend we had to go back to `hlint` version `2.2.10`.
We can change `version` and temporarily add
`checkMaterialization = true;`:

```nix
let inherit (import ./. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
    hlint = pkgs.haskell-nix.hackage-package {
      name = "hlint";
      version = "2.2.10";
      index-state = "2020-04-15T00:00:00Z";
      plan-sha256 = "02hasr27a994sml1fzf8swb716lm6lgixxr53y0gxkhw437xkck4";
      materialized = ./hlint.materialized;
      checkMaterialization = true;
    };
in hlint.components.exes.hlint
```

This will fail and report the details of what is wrong:

```
$ nix-build hlint.nix
trace: Using index-state: 2020-04-15T00:00:00Z for hlint
building '/nix/store/575g3fxn99jyxg50x5mfin2nk1n831cm-hlint-plan-to-nix-pkgs.drv'...
Changes to hlint-plan-to-nix-pkgs not reflected in plan-sha256
diff -ru /nix/store/kk047cqsjvbj4w8psv4l05abdcnyrqdc-hlint-plan-to-nix-pkgs/.plan.nix/hlint.nix /nix/store/ywdhbx9rzzkfc60c5vzk7cins2hnvkgx-hlint-plan-to-nix-pkgs/.plan.nix/hlint.nix
--- /nix/store/kk047cqsjvbj4w8psv4l05abdcnyrqdc-hlint-plan-to-nix-pkgs/.plan.nix/hlint.nix      1970-01-01 00:00:01.000000000 +0000
+++ /nix/store/ywdhbx9rzzkfc60c5vzk7cins2hnvkgx-hlint-plan-to-nix-pkgs/.plan.nix/hlint.nix      1970-01-01 00:00:01.000000000 +0000
@@ -42,7 +42,7 @@
     flags = { threaded = true; gpl = true; ghc-lib = false; };
     package = {
       specVersion = "1.18";
-      identifier = { name = "hlint"; version = "2.2.11"; };
+      identifier = { name = "hlint"; version = "2.2.10"; };
       license = "BSD-3-Clause";
       copyright = "Neil Mitchell 2006-2020";
       maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
@@ -103,7 +103,6 @@
           then [
             (hsPkgs."ghc" or (buildDepError "ghc"))
             (hsPkgs."ghc-boot-th" or (buildDepError "ghc-boot-th"))
-            (hsPkgs."ghc-boot" or (buildDepError "ghc-boot"))
             ]
           else [
             (hsPkgs."ghc-lib-parser" or (buildDepError "ghc-lib-parser"))
diff -ru /nix/store/kk047cqsjvbj4w8psv4l05abdcnyrqdc-hlint-plan-to-nix-pkgs/default.nix /nix/store/ywdhbx9rzzkfc60c5vzk7cins2hnvkgx-hlint-plan-to-nix-pkgs/default.nix
--- /nix/store/kk047cqsjvbj4w8psv4l05abdcnyrqdc-hlint-plan-to-nix-pkgs/default.nix      1970-01-01 00:00:01.000000000 +0000
+++ /nix/store/ywdhbx9rzzkfc60c5vzk7cins2hnvkgx-hlint-plan-to-nix-pkgs/default.nix      1970-01-01 00:00:01.000000000 +0000
@@ -2,7 +2,7 @@
   pkgs = hackage:
     {
       packages = {
-        "ghc-lib-parser-ex".revision = (((hackage."ghc-lib-parser-ex")."8.8.5.8").revisions).default;
+        "ghc-lib-parser-ex".revision = (((hackage."ghc-lib-parser-ex")."8.8.4.0").revisions).default;
         "ghc-lib-parser-ex".flags.ghc-lib = false;
         "exceptions".revision = (((hackage."exceptions")."0.10.4").revisions).default;
         "exceptions".flags.transformers-0-4 = true;
Calculated hash is 0zsi3wv92qax33ic4n5dfsqd1r9qam1k75za3c5jqgdxl3hy8vph expected hash was 02hasr27a994sml1fzf8swb716lm6lgixxr53y0gxkhw437xkck4 for hlint-plan-to-nix-pkgs
builder for '/nix/store/575g3fxn99jyxg50x5mfin2nk1n831cm-hlint-plan-to-nix-pkgs.drv' failed with exit code 1
error: build of '/nix/store/575g3fxn99jyxg50x5mfin2nk1n831cm-hlint-plan-to-nix-pkgs.drv' failed
(use '--show-trace' to show detailed location information)
```

Checking the materialization requires nix to do all the work that materialization
avoids.  So while it might be tempting to leave `checkMaterialization = true` all
the time, we would be better off just removing `materialized` and `plan-sha256`.

## How can we update the nix files with a script?

There are versions of the functions (`cabalProject'`, `stackProject'`
and `hackage-project`) that also return the nix as `plan-nix` or
`stack-nix`.  By calling one of these functions without the
hash and materialized nix we can find out what nix files should be.
For instance:

```nix
let inherit (import ./. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
    hlint = pkgs.haskell-nix.hackage-project {
      name = "hlint";
      version = "2.2.10";
      index-state = "2020-04-15T00:00:00Z";
    };
in hlint
```

```
$ nix-build hlint.nix -A plan-nix
trace: Using index-state: 2020-04-15T00:00:00Z for hlint
trace: Get `plan-sha256` with `nix-hash --base32 --type sha256 /nix/store/ywdhbx9rzzkfc60c5vzk7cins2hnvkgx-hlint-plan-to-nix-pkgs/`
/nix/store/ywdhbx9rzzkfc60c5vzk7cins2hnvkgx-hlint-plan-to-nix-pkgs
```

We can have the script copy `$(nix-build hlint.nix -A plan-nix --no-out-link)`
and use `nix-hash` to calculate the new value for `plan-sha256`.

## Can we skip making a copy and use `materialized = /nix/store/...`?

Yes and it gives us the same speed improvement, however:

* It does not help at all in `restricted-eval` mode (Hydra).

* Users will still wind up building or downloading the dependencies
  needed to build the nix fileds (if they do not have them).

For those reasons it might be best to make a copy instead
of using the `/nix/store/...` path directly.

If you really want to use the `/nix/store/...` path directly
you should gaurd against the path not existing as passing in
a non-existing path is now an error:

```nix
let inherit (import ./. {}) sources nixpkgsArgs;
    pkgs = import sources.nixpkgs-default nixpkgsArgs;
    hlintPlan = /nix/store/kk047cqsjvbj4w8psv4l05abdcnyrqdc-hlint-plan-to-nix-pkgs;
    hlint = pkgs.haskell-nix.hackage-package {
      name = "hlint";
      version = "2.2.11";
      index-state = "2020-04-15T00:00:00Z";
      plan-sha256 = "02hasr27a994sml1fzf8swb716lm6lgixxr53y0gxkhw437xkck4";
      materialized = if __pathExists hlintPlan then hlintPlan else null;
    };
in hlint.components.exes.hlint
```

Running when no building is needed is still slow in restricted evaluation mode.

```
$ time nix-build --option restrict-eval true -I . --option allowed-uris "https://github.com/NixOS https://github.com/input-output-hk" hlint.nix --show-trace
trace: Using index-state: 2020-04-15T00:00:00Z for hlint
/nix/store/rnfz66v7k8i38c8rsmchzsyqjrmrbdpk-hlint-2.2.11-exe-hlint

real	0m4.463s
user	0m4.440s
sys	0m0.461s
$ time nix-build hlint.nix
/nix/store/rnfz66v7k8i38c8rsmchzsyqjrmrbdpk-hlint-2.2.11-exe-hlint

real	0m2.206s
user	0m1.665s
sys	0m0.332s
```
