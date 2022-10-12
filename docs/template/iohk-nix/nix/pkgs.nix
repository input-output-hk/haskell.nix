{ pkgs ? import <nixpkgs> {}
, iohk-extras ? {}
, iohk-module ? {}
, haskell
, hackage
, stackage
, ...
}:
let
  # our packages
  stack-pkgs = import ./.stack-pkgs.nix;

  # Build the packageset with module support.
  # We can essentially override anything in the modules
  # section.
  #
  #  packages.cbors.patches = [ ./one.patch ];
  #  packages.cbors.flags.optimize-gmp = false;
  #
  compiler = (stack-pkgs.extras hackage).compiler.nix-name;
  pkgSet = haskell.mkNewPkgSet {
    inherit pkgs;
    pkg-def = stackage.${stack-pkgs.resolver};
    # These extras allow extension or restriction of the set of
    # packages we are interested in. By using the stack-pkgs.extras
    # we restrict our package set to the ones provided in stack.yaml.
    pkg-def-extras = [
      stack-pkgs.extras
      iohk-extras.${compiler}
    ];
    # package customizations
    modules = [
      # This module will ensure that we get the necessary
      # patches ontop of GHC packages that for which the
      # ones that GHC ships are not identical to the ones
      # we find on hackage. These patches will make sure
      # they are identical by augmenting the packages on
      # hackage to match those that ship with ghc.
      haskell.ghcHackagePatches.${compiler}

      # the iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.  For now we need to
      # list the packages that require template haskell
      # explicity here.
      iohk-module
    ];
  };
in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }