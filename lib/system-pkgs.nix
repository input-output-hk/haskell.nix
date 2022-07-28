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

  # Apply the mapping.
  // import ./system-nixpkgs-map.nix pkgs
