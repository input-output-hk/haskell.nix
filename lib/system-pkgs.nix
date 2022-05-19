# Cabal packages can depend on operating system packages. However
# package names vary between distributions - there is no standard.
#
# This file augments the nixpkgs package set with extra attributes
# from the system packages map file.
#
# This package set will be used when resolving system package
# dependencies of package descriptions.

pkgs:

  # Base packages.
  pkgs

  # fetchgit should always come from the evalPackages
  # if it comes from the targetPackages we won't even
  # be able to execute it.
  // { fetchgit = pkgs.evalPackages.fetchgit; }

  # Apply the mapping.
  // import ./system-nixpkgs-map.nix pkgs
