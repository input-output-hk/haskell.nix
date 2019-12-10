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
((import ./nixpkgs (import ./.)).haskell-nix.hackage-package {
  name = "hlint";
  version = "2.2.4";
}).components.exes.hlint
```

Building this may result in a lot of output, but if youb build
it again it should give just:

```
$ nix-build hlint.nix 
trace: Using latest index state for hlint!
trace: Using index-state: 2019-12-09T00:00:00Z for hlint
/nix/store/7wwqq5v42gm6iiz2d3ngbnkyw7i4py11-hlint-2.2.4-exe-hlint
```

To materialize the nix files we need to take care to pin down the
inputs.  For `cabalProject` and `hackage-package` this means
we must specify the `index-state` of hackage we want to use:

```nix
((import ./nixpkgs (import ./.)).haskell-nix.hackage-package {
  name = "hlint";
  version = "2.2.4";
  index-state = "2019-12-03T00:00:00Z";
}).components.exes.hlint
```

Now if we build again we get a hint telling use how to
calculate a suitable sha256 hash to turn the derivation
containing the nix files into a fixed output derivation:

```
$ nix-build hlint.nix 
trace: Using index-state: 2019-12-03T00:00:00Z for hlint
trace: Get `plan-sha256` with `nix-hash --base32 --type sha256 /nix/store/qk1fvza1alkvs51vzmpjp2xsg8xklyxk-hlint-plan-to-nix-pkgs/`
/nix/store/7wwqq5v42gm6iiz2d3ngbnkyw7i4py11-hlint-2.2.4-exe-hlint

$ nix-hash --base32 --type sha256 /nix/store/qk1fvza1alkvs51vzmpjp2xsg8xklyxk-hlint-plan-to-nix-pkgs/
1a4rhv3h2daz6dzwzfl3w7l1v556n7aqfiagw6m0rvqq230iabss
```

We can add the hash as `plan-sha256` or (`stack-sha256` for
`stackProject`)

```nix
((import ./nixpkgs (import ./.)).haskell-nix.hackage-package {
  name = "hlint";
  version = "2.2.4";
  index-state = "2019-12-03T00:00:00Z";
  plan-sha256 = "1a4rhv3h2daz6dzwzfl3w7l1v556n7aqfiagw6m0rvqq230iabss";
}).components.exes.hlint
```

Just adding the hash might help reuse of the cached nix, but nix will
still calculate all the dependencies (which can add seconds to
`nix-build` and `nix-shell` commands when no ohter work is needed)
and users who do not yet have the dependencies in their store will have
to wait while they are built or downloaded.

Running nix build again gives us a hint on what we can do next:

```
$ nix-build hlint.nix 
trace: Using index-state: 2019-12-03T00:00:00Z for hlint
trace: To materialize copy /nix/store/0xalcphb7ifvy5fc9dpwj40fij6nn5av-hlint-plan-to-nix-pkgs
/nix/store/7wwqq5v42gm6iiz2d3ngbnkyw7i4py11-hlint-2.2.4-exe-hlint
```

To capture the nix we can do something like:

```nix
((import ./nixpkgs (import ./.)).haskell-nix.hackage-package {
  name = "hlint";
  version = "2.2.4";
  index-state = "2019-12-03T00:00:00Z";
  plan-sha256 = "1a4rhv3h2daz6dzwzfl3w7l1v556n7aqfiagw6m0rvqq230iabss";
  materialized = ./hlint.materialized;
}).components.exes.hlint
```

Now we can copy the nix files needed and build with:

```
$ cp -r /nix/store/0xalcphb7ifvy5fc9dpwj40fij6nn5av-hlint-plan-to-nix-pkgs hlint.materialized
$ nix-build hlint.nix 
/nix/store/7wwqq5v42gm6iiz2d3ngbnkyw7i4py11-hlint-2.2.4-exe-hlint
```

We may want to run `cmod -R +w hlint.materialized` as the files copied from the
store will be read only.

## How can we check `sha256` and `materialized` are up to date?

Let's pretend we had to go back to `hlint` version `2.2.3`.
We can change `version` and temporarily add
`checkMaterialization = true;`:

```nix
((import ./nixpkgs (import ./.)).haskell-nix.hackage-package {
  name = "hlint";
  version = "2.2.3";
  index-state = "2019-12-03T00:00:00Z";
  plan-sha256 = "1a4rhv3h2daz6dzwzfl3w7l1v556n7aqfiagw6m0rvqq230iabss";
  materialized = ./hlint.materialized;
  checkMaterialization = true;
}).components.exes.hlint
```

This will fail and report the details of what is wrong:

```
$ nix-build hlint.nix 
trace: Using index-state: 2019-12-03T00:00:00Z for hlint
building '/nix/store/zmif4gk52ynh57pf4dikzgsk30haqi2b-hlint-plan-to-nix-pkgs.drv'...
Changes to hlint-plan-to-nix-pkgs not reflected in plan-sha256
diff -ru /nix/store/0xalcphb7ifvy5fc9dpwj40fij6nn5av-hlint-plan-to-nix-pkgs/.plan.nix/hlint.nix /nix/store/h5j8k3y5lansyfss25gd7knbninzr6z4-hlint-plan-to-nix-pkgs/.plan.nix/hlint.nix
--- /nix/store/0xalcphb7ifvy5fc9dpwj40fij6nn5av-hlint-plan-to-nix-pkgs/.plan.nix/hlint.nix      1970-01-01 00:00:01.000000000 +0000
+++ /nix/store/h5j8k3y5lansyfss25gd7knbninzr6z4-hlint-plan-to-nix-pkgs/.plan.nix/hlint.nix      1970-01-01 00:00:01.000000000 +0000
@@ -42,7 +42,7 @@
     flags = { threaded = true; gpl = true; ghc-lib = false; };
     package = {
       specVersion = "1.18";
-      identifier = { name = "hlint"; version = "2.2.4"; };
+      identifier = { name = "hlint"; version = "2.2.3"; };
       license = "BSD-3-Clause";
       copyright = "Neil Mitchell 2006-2019";
       maintainer = "Neil Mitchell <ndmitchell@gmail.com>";
@@ -95,7 +95,6 @@
           (hsPkgs."extra" or (buildDepError "extra"))
           (hsPkgs."refact" or (buildDepError "refact"))
           (hsPkgs."aeson" or (buildDepError "aeson"))
-          (hsPkgs."filepattern" or (buildDepError "filepattern"))
           (hsPkgs."syb" or (buildDepError "syb"))
           (hsPkgs."mtl" or (buildDepError "mtl"))
           ] ++ (if !flags.ghc-lib && (compiler.isGhc && (compiler.version).ge "8.8.0") && (compiler.isGhc && (compiler.version).lt "8.9.0")
diff -ru /nix/store/0xalcphb7ifvy5fc9dpwj40fij6nn5av-hlint-plan-to-nix-pkgs/default.nix /nix/store/h5j8k3y5lansyfss25gd7knbninzr6z4-hlint-plan-to-nix-pkgs/default.nix
--- /nix/store/0xalcphb7ifvy5fc9dpwj40fij6nn5av-hlint-plan-to-nix-pkgs/default.nix      1970-01-01 00:00:01.000000000 +0000
+++ /nix/store/h5j8k3y5lansyfss25gd7knbninzr6z4-hlint-plan-to-nix-pkgs/default.nix      1970-01-01 00:00:01.000000000 +0000
@@ -76,7 +76,7 @@
         "tagged".revision = (((hackage."tagged")."0.8.6").revisions).default;
         "tagged".flags.transformers = true;
         "tagged".flags.deepseq = true;
-        "haskell-src-exts".revision = (((hackage."haskell-src-exts")."1.22.0").revisions).default;
+        "haskell-src-exts".revision = (((hackage."haskell-src-exts")."1.21.1").revisions).default;
         "unliftio-core".revision = (((hackage."unliftio-core")."0.1.2.0").revisions).default;
         "ghc-lib-parser".revision = (((hackage."ghc-lib-parser")."8.8.1").revisions).default;
         "containers".revision = (((hackage."containers")."0.6.0.1").revisions).default;
@@ -116,7 +116,6 @@
         "hpc".revision = (((hackage."hpc")."0.6.0.3").revisions).default;
         "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
         "process".revision = (((hackage."process")."1.6.5.0").revisions).default;
-        "filepattern".revision = (((hackage."filepattern")."0.1.1").revisions).default;
         "libyaml".revision = (((hackage."libyaml")."0.1.1.1").revisions).default;
         "libyaml".flags.system-libyaml = false;
         "libyaml".flags.no-unicode = false;
Calculated hash is 1qjmhlb4rw6mggs7y57f6zr5zjmkhkx7sn9q8pb18308n5nxgxcs expected hash was 1a4rhv3h2daz6dzwzfl3w7l1v556n7aqfiagw6m0rvqq230iabss for hlint-plan-to-nix-pkgs
builder for '/nix/store/zmif4gk52ynh57pf4dikzgsk30haqi2b-hlint-plan-to-nix-pkgs.drv' failed with exit code 1
error: build of '/nix/store/zmif4gk52ynh57pf4dikzgsk30haqi2b-hlint-plan-to-nix-pkgs.drv' failed
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
(import ./nixpkgs (import ./.)).haskell-nix.hackage-project {
  name = "hlint";
  version = "2.2.4";
  index-state = "2019-12-03T00:00:00Z";
}
```

```
$ nix-build hlint.nix -A plan-nix
trace: Using index-state: 2019-12-03T00:00:00Z for hlint
trace: Get `plan-sha256` with `nix-hash --base32 --type sha256 /nix/store/qk1fvza1alkvs51vzmpjp2xsg8xklyxk-hlint-plan-to-nix-pkgs/`
/nix/store/qk1fvza1alkvs51vzmpjp2xsg8xklyxk-hlint-plan-to-nix-pkgs
```

We can have the script copy `$(nix-build hlint.nix -A plan-nix --no-out-link)`
and use `nix-hash` to calculate the new value for `plan-sha256`.

## Can we skip making a copy and use `materialized = /nix/store/...`?

Yes and it gives us the same speed improvement, however:

* It does not help at all in `restricted-eval` mode (Hydra).

* Users will still wind up building or downloading the dependencies
  needed to build the nix fileds (if they do not have them).

For instance:

```nix
((import ./nixpkgs (import ./.)).haskell-nix.hackage-package {
  name = "hlint";
  version = "2.2.4";
  index-state = "2019-12-03T00:00:00Z";
  plan-sha256 = "1a4rhv3h2daz6dzwzfl3w7l1v556n7aqfiagw6m0rvqq230iabss";
  materialized = /nix/store/qk1fvza1alkvs51vzmpjp2xsg8xklyxk-hlint-plan-to-nix-pkgs;
}).components.exes.hlint
```

Running when no building is needed is still slow in restricted evaluation mode.

```
$ time nix-build --option restrict-eval true -I . --option allowed-uris "https://github.com/NixOS https://github.com/input-output-hk" hlint.nix 
trace: Using index-state: 2019-12-03T00:00:00Z for hlint
/nix/store/7wwqq5v42gm6iiz2d3ngbnkyw7i4py11-hlint-2.2.4-exe-hlint

real	0m10.066s
user	0m8.563s
sys	0m0.630s
$ time nix-build hlint.nix 
/nix/store/7wwqq5v42gm6iiz2d3ngbnkyw7i4py11-hlint-2.2.4-exe-hlint

real	0m4.628s
user	0m3.889s
sys	0m0.389s
```
