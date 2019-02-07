# haskell.nix

haskell.nix is an alternative Haskell infrastructure for nix.

## Motivation

Why do we need another Haskell infrastructure for nix?  Doesn't nixpkgs
provide a sufficiently good Haskell infrastructure already?  These are
good questions.  And it boils down to the following reasons for us to
embark on a new infrastructure:

- first class support for cross compilation
- first class support for package sets
- component level control when building packages
- reduction of `dontCheck` for cyclic dependencies
- reducing build times by building libraries and tests in parallel
- moving more logic into nix
- decoupling of Haskell and nixpkgs

### cross compilation

`nixpkgs` has quite good support for cross compilation, however the
Haskell infrastructure suffers from the fact that it heavily relies on
the `cabal2nix` tool.  `cabal2nix` (as well as tools that depend on it
like `stack2nix`) flattens the `.cabal` file at conversion time to a
given os/arch/flags configuration.  Thus to make cross compilation
work with `cabal2nix` you will have to generate a separate `nix`
expression for each configuration.  This becomes a major maintenance
burden over time.  Therefore the tooling that translates cabal files
into nix-expressions for use with Haskell.nix retains the full
conditional tree from the cabal file and exposes it to `nix`.  In
addition it will also expose the `build-type` value, which allows us
to cache the `Setup.hs` for build-type simple and not have to rebuild
it every time.

### package sets

We often rely on either package sets as provided by stackage or
computed by cabal.  `nixpkgs` provides it's own curated package set
which might or might not work for the projects we work on.
`stack2nix` tries to solve this issue, here we go one step further and
provide the infrastructure to allow any form of package set.

### component level control

The Haskell builder in `nixpkgs` provides control over executables and
libraries, to build a specific executable only however is rather
tricky to do.  This also leads to the cyclic dependencies issue.

### cyclic dependencies

Because the Haskell builder in `nixpkgs` exposes packages at the
package level, if packages mutually depend on each other through tests
and libraries, lead to cyclic dependencies that nix can't resolve.  By
exposing the components to nix as separate derivations this will only
occur if you have mutually dependent components.

### build times

The Haskell builder in nixpkgs build package sequentially, first the
library than the executables and finally the tests.  It then executes
the tests before the package is considered done.  The upshot of this
is that packages are only considered done if the test-suites
passed.  The downside is that if you have to compile multiple packages
the likelihood of them failing is low, you have unnecessarily
serialized you build.  In a more aggressive setting libraries could
start building as early as their dependent libraries are built.  Of
course they will have to be invalidated later should the test-suites
of their dependencies fail, but this way we can make use of parallel
building.  In an ideal scenario this will reduce build times close to
the optimum.

### more logic in nix

The `cabal2nix` tool has a resolver that resolved system dependencies
and licenses to values in `nixpkgs`.  This logic end up being a simple
dictionary lookup and can be a simple nix expression.  This also
offloads some of the work the cabal to nix translation tool needs to
do into nix, and as such if changes are necessary (or needed to be
performed ad hoc) there is no need to rebuild the conversion tool and
subsequently mark every derived expression as out of date.

### decoupleing

Finally by treating Haskell.nix and nixpkgs as separate entities we
can decouple the Haskell packages and infrastructure from the nixpkgs
package set, and rely on it to provide us with system packages while
staying up to date with Haskell packages from hackage while retaining
a stable (or known to be good) nixpkgs revision.
