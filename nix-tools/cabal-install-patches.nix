# haskell.nix module that patches the `cabal-install` library used by
# nix-tools' `make-install-plan`.  Applied to both the regular
# nix-tools build (`nix-tools/overlay.nix`) and the static one
# (`nix-tools/static/project.nix`) — keeping the reference in one
# place so it can't drift between the two.
#
# The patch makes `Distribution.Client.PackageHash`'s
# `hashedInstalledPackageId` consult `CABAL_INSTALLED_PACKAGE_ID_OS`,
# pinning the unit-id format to the *build* platform's OS.  Without
# it, plan-nix unit-ids fork from slice-build unit-ids whenever the
# eval system differs from the build system (e.g. evaluating on
# Darwin while building x86_64-linux derivations).
{
  packages.cabal-install.patches = [
    ../builder/cabal-install-patches/installed-package-id-os-override.patch
  ];
}
