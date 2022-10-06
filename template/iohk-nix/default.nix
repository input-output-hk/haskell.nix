let
  localLib = import ./lib.nix;
in
# This file needs to export a function that takes
# the arguments it is passed and forwards them to
# the default-nix template from iohk-nix. This is
# important so that the release.nix file can properly
# parameterize this file when targetting different
# hosts.
{ ... }@args:
# We will instantiate the defaul-nix template with the
# nix/pkgs.nix file...
localLib.nix-tools.default-nix ./nix/pkgs.nix args
# ... and add a few custom packages as well.
// { }