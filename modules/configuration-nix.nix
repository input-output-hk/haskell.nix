# This file is for configuration that should be required by every
# package set. Hopefully we can keep configuration for particular
# package sets out of this repo. Ideally, this file is only used for
# fixing things that are broken due to the Nix infrastructure.

{ pkgs, ... }:
let
  fromUntil = from: until: patch: { version, revision }:
    if   builtins.compareVersions version from  >= 0
      && builtins.compareVersions version until <  0
      then patch
      else null;
in {
  # terminfo doesn't list libtinfo in its cabal file. We could ignore
  # this if we used the terminfo shipped with GHC, but this package is
  # reinstallable so we'd rather have it defined in the plan.
  packages.terminfo.components.library.libs = [ pkgs.ncurses ];

  # The `extra-libraries` field in `X11.cabal` does not include Xss and Xinerama
  # see https://github.com/input-output-hk/haskell.nix/pull/988
  packages.X11.components.library.libs = [
    pkgs.xorg.libXScrnSaver
    pkgs.xorg.libXinerama
  ];

  # odbc needs this package to provide odbcss.h on Linux and macOS, see
  # https://github.com/fpco/odbc#common-issues
  packages.odbc.components.library.libs = [ pkgs.freetds ];

  # These packages have `license: LGPL` in their .cabal file, but
  # do not specify the version.  Setting the version here on
  # examination of the license files included in the packages.
  packages.hscolour.package.license = pkgs.lib.mkForce "LGPL-2.1-only";
  packages.cpphs.package.license = pkgs.lib.mkForce "LGPL-2.1-only";
  packages.polyparse.package.license = pkgs.lib.mkForce "LGPL-2.1-only";

  # These two patches are needed by GHCJS
  packages.Cabal.patches = [
    (fromUntil "3.2.0.0" "3.5" ../overlays/patches/Cabal/Cabal-3.0.0.0-drop-pkg-db-check.diff)
    (fromUntil "3.2.0.0" "3.5" ../overlays/patches/Cabal/Cabal-3.0.0.0-no-final-checks.diff)
    (fromUntil "3.6.0.0" "3.7" ../overlays/patches/Cabal/Cabal-3.6.0.0-drop-pkg-db-check.diff)
    (fromUntil "3.6.0.0" "3.7" ../overlays/patches/Cabal/Cabal-3.6.0.0-no-final-checks.diff)
  ];

  # These two patches are:
  #   https://github.com/haskell/cabal/pull/7490
  #   https://github.com/haskell/cabal/pull/7532
  # back poerted to cabal 3.4
  packages.cabal-install.patches = [
    (fromUntil "3.4.0.0" "3.5" ../overlays/patches/Cabal/Cabal-3.4-defer-build-tool-depends-7532.patch)
    (fromUntil "3.4.0.0" "3.5" ../overlays/patches/Cabal/Cabal-3.4-speedup-solver-when-tests-enabled-7490.patch)
  ];
  # Remove dependency on hsc2hs (hsc2hs should be in ghc derivation)
  packages.mintty.components.library.build-tools = pkgs.lib.mkForce [];

  packages.ghc-lib-parser.patches = [
    (fromUntil "8.10.0.0" "9.1" ../overlays/patches/ghc-lib-parser-8.10-global-unique-counters-in-rts.patch)
  ];
}
