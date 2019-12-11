# cleanGit

To filter out just the files in your git index use
`haskell-nix.haskellLib.cleanGit { src = ./.; }` where `./.` is the
root of your git repo (or a git work tree).

First it filters just the files needed to run `git index`, then
it uses the results of that to filter your directory.  It does not
need to parse the `.gitignore` files at all, but we do need to
`git add` our files before they will be included.
[cleanGit source](https://github.com/input-output-hk/haskell.nix/blob/master/lib/clean-git.nix).

In addition haskell.nix (including `cleanGit`) uses a version
of [`cleanSourceWith`](https://github.com/input-output-hk/haskell.nix/blob/master/lib/clean-source-with.nix)
with a `subdir` argument to filter out just the package it is
building.  Then it uses the info from the `cabal` file to filter
just the source dirs for the component it is building.  That way
if we modify a test in a package nix will not rebuild the library
in that package (or anything that depends on that package's library).

There is a downside to this though.  If we have a test that depends
on something outside the scope of what is described in its entry in
the in the `.cabal` file it will not see it.  For instance perhaps
it needs to run `hlint` or `doctest` on the library source.  There
are ways to fix this with a module:

Use `extraSrcFiles` to add dirs the test needs (this will not result
in a change to the `.cabal` file the test will still be built the same).
```
components.tests.test.extraSrcFiles = [ "subdir-needed-by-test" ];
```
Or alternatively, override the source with a suitable filter function. 
```
components.tests.test.src = haskell-nix.haskellLib.cleanSourceWith {
    inherit src;
    subdir = "path-to-package";
    filter = ...
};
```

