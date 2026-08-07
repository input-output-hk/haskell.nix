
# Pin for the *stable-haskell* static nix-tools, deliberately separate from
# ./nix-tools-static.nix.
#
# The stable-haskell tools are built from ./nix-tools-sh, against the
# stable-haskell Cabal fork (3.17, branch hkm/installed-sublibs) rather than
# Cabal 3.16, with matching source changes in Cabal2Nix / MakeInstallPlan /
# ProjectPlanOutput / Freeze / setup-ghcjs.  plan-to-nix and make-install-plan
# generate plan-nix for whatever compiler they are pointed at, so folding these
# into the shared pin would put every existing GHC on an experimental
# toolchain.  This pin exists so ghc914-sh (and only it) can opt in through
# `haskell-nix.nix-tools-unchecked-sh`, while ghc9.6 .. ghc9.14 keep using
# ./nix-tools-static.nix untouched.
#
# Released by .github/workflows/upload-artifacts-sh.yml on `nix-tools-sh-*`
# tags, which overwrites this file.  Until the first such tag is cut there is
# nothing to point at.  Throwing keeps the tree evaluable -- nix forces this
# only when something actually asks for the stable-haskell tools -- and makes
# the failure explain itself instead of 404ing inside a fetchurl.

_pkgs:

throw ''
  No stable-haskell static nix-tools has been released yet, so
  `haskell-nix.nix-tools-unchecked-sh` cannot be resolved.

  To create one, push a `nix-tools-sh-<version>` tag on a master commit.  The
  tools are built from ./nix-tools-sh in this repository, so — unlike the
  mainline `nix-tools-*` tags — no separate branch or Hydra jobset is needed:
  .github/workflows/upload-artifacts-sh.yml reads the four
  `<system>.nix-tools-sh.static.zipped.*` jobs out of the master evaluation for
  the tagged revision, publishes the release, and opens the PR that replaces
  this file.

  If you did not mean to use the stable-haskell tools, something has set
  `nix-tools` to `nix-tools-unchecked-sh`.  The shared pin is
  ./nix-tools-static.nix, reached through `nix-tools-unchecked`.
''
