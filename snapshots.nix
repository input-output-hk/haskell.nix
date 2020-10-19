# This provides a package set for each snapshot in Stackage.
#
# It allows you to use a bare snapshot without having to invoke
# mkStackPkgSet with a stack.yaml project.
#
# A particular package in a snapshot would be accessed with:
#   snapshots."lts-13.18".conduit

{ lib, mkPkgSet, stackage, excludeBootPackages, ghc-boot-packages }:

with lib;

let
  mkSnapshot = name: pkg-def: (let pkgSet = mkPkgSet {
    pkg-def = excludeBootPackages null pkg-def;
    # ghc-boot-packages are needed for the reinstallable ghc library and
    # are constructed from the patched ghc source.
    pkg-def-extras = (pkg-def-extras name)
      ++ [(hackage: ghc-boot-packages.${(pkg-def hackage).compiler.nix-name})];
    modules = [
      { reinstallableLibGhc = true; } # Allow ghc library to be installed for packages that need it
      { packages.alex.package.setup-depends = [pkgSet.config.hsPkgs.Cabal]; }
      { packages.happy.package.setup-depends = [pkgSet.config.hsPkgs.Cabal]; }
    ] ++ optional (ltsInRange "1" "15" name) {
        packages.Cabal.patches = [ ./overlays/patches/Cabal/fix-data-dir.patch ];
    };
  }; in pkgSet).config.hsPkgs;

  # Tests whether snapshot name is an LTS within
  # the half-open version interval [start, end).
  ltsInRange = start: end: name: let
    components = splitString "-" name;
    version = concatStringsSep "-" (drop 1 components);
  in
    assert length components >= 2;
    head components == "lts"
    && versionAtLeast version start
    && versionOlder version end;

  # A function to get pkg-def-extras with build fixes for certain
  # snapshots.
  pkg-def-extras = let
    fixes = {
      # Work around a mismatch between stackage metadata and the
      # libraries shipped with GHC.
      # https://github.com/commercialhaskell/stackage/issues/4466
      fix-ghc-transformers = {
        predicate = ltsInRange "12" "14"; # [12, 14) : 14.1 has correct versions
        extra = hackage: {
          packages = {
            "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
            "process" = (((hackage.process)."1.6.5.0").revisions).default;
          };
        };
      };

      # Add hsc2hs to the snapshot. This is a build tool for many
      # packages. Stackage does not include it in the snapshots
      # because it is expected that hsc2hs comes with ghc.
      fix-hsc2hs = {
        predicate = ltsInRange "1" "14"; # [1, 14) : 14.1 includes hsc2hs
        extra = hackage: {
          packages = {
            "hsc2hs" = (((hackage.hsc2hs)."0.68.4").revisions).default;
          };
        };
      };
    };

    applyFix = name: fix: optional ((fix.predicate or (const true)) name) fix.extra;

  in
    name: concatLists (mapAttrsToList (_: applyFix name) fixes);

in
  mapAttrs mkSnapshot stackage
