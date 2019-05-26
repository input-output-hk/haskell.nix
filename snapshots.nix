{ lib, mkPkgSet, stackage }:

with lib;

let
  mkSnapshot = name: pkg-def: (mkPkgSet {
    inherit pkg-def;
    pkg-def-extras = pkg-def-extras name;
  }).config.hsPkgs;

  # Tests whether snapshot name is an LTS within
  # the half-open version interval [start, end).
  ltsInRange = start: end: name: let
    components = splitString "-" name;
    version = last components;
  in
    assert length components == 2;
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
        predicate = ltsInRange "12" "14";
        extra = hackage: {
          packages = {
            "transformers" = (((hackage.transformers)."0.5.6.2").revisions).default;
            "process" = (((hackage.process)."1.6.5.0").revisions).default;
          };
        };
      };
    };
  in
    name: concatLists (mapAttrsToList (_: fix: optional (fix.predicate name) fix.extra) fixes);

in
  mapAttrs mkSnapshot stackage
