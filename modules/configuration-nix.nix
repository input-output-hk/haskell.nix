# This file is for configuration that should be required by every
# package set. Hopefully we can keep configuration for particular
# package sets out of this repo. Ideally, this file is only used for
# fixing things that are broken due to the Nix infrastructure.

{ pkgs, config, ... }:
let
  fromUntil = from: until: patch: { version, revision }:
    if   builtins.compareVersions version from  >= 0
      && builtins.compareVersions version until <  0
      then patch
      else null;
  from = v: patch: { version, revision }:
    if builtins.compareVersions version v >= 0
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
    (fromUntil "3.6.0.0" "3.9" ../overlays/patches/Cabal/Cabal-3.6.0.0-drop-pkg-db-check.diff)
    (fromUntil "3.6.0.0" "3.9" ../overlays/patches/Cabal/Cabal-3.6.0.0-no-final-checks.diff)
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
    (fromUntil "8.10.0.0" "9.2" ../overlays/patches/ghc-lib-parser-8.10-global-unique-counters-in-rts.patch)
    (fromUntil "9.2.0.0" "9.3" ../overlays/patches/ghc-lib-parser-9.2-global-unique-counters-in-rts.patch)
    (fromUntil "9.4.0.0" "9.5" ../overlays/patches/ghc-lib-parser-9.4-global-unique-counters-in-rts.patch)
  ];

  # See https://github.com/haskell-nix/hnix/pull/1053
  packages.hnix.patches = [
    (fromUntil "0.16.0" "0.16.0.1" ../patches/hnix.patch)
  ];

  # See https://github.com/input-output-hk/haskell.nix/issues/1455
  # This is a work around to make `ghcide` and `haskell-language-server` build with the unboxed tuple patch.
  packages.ghcide.patches =
    # Work out if we have applied the unboxed tupple patch in overlays/bootstrap.nix
    pkgs.lib.optional (__elem config.compiler.nix-name [
      "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc8105" "ghc8106" "ghc8107" "ghc810420210212"
    ]) (from "1.7.0.0" ../patches/ghcide-1.7-unboxed-tuple-fix-issue-1455.patch)
    # This is needed for a patch only applied to ghc810420210212
    ++ pkgs.lib.optional (__elem config.compiler.nix-name [
      "ghc810420210212"
    ]) (from "1.7.0.0" ../patches/ghcide-1.7-plutus-ghc.patch);

  packages.language-c.patches = [
    # See https://github.com/visq/language-c/pull/89
    # this adds support for __int128_t and __uint128_t to language-c
    (fromUntil "0.9.1" "0.9.2" ../patches/languge-c-int128.patch)
  ];

  packages.discount.components.library.libs = pkgs.lib.mkForce [ pkgs.discount ];

  packages.llvm-hs.components.library.build-tools = pkgs.lib.mkForce [ pkgs.llvm ];

  packages.BNFC.components.tests.doctests.build-tools = [
    config.hsPkgs.buildPackages.alex
    config.hsPkgs.buildPackages.happy
  ];

  packages.Sit.components.library.build-tools = [
    # These may not be in `config.hsPkgs.buildPackages` are not even included
    # as dependencies of the Sit package at all.
    (pkgs.buildPackages.haskell-nix.tool config.compiler.nix-name "alex" {})
    (pkgs.buildPackages.haskell-nix.tool config.compiler.nix-name "happy" {})
  ];

  packages.bindings-GLFW.components.library.libs = [
    pkgs.xorg.libXext
  ];

  packages.GLFW-b.components.library.libs = [
    pkgs.xorg.libXi
  ];

  packages.closed.components.tests.readme.build-tools = [
    config.hsPkgs.buildPackages.markdown-unlit
  ];

  # Build ghci and ghc with internal interpreter support to make the
  # `reinstallableLibGhc` build more like the boot version.
  # See https://github.com/input-output-hk/haskell.nix/issues/1512
  packages.ghc.flags.ghci = true;
  packages.ghc.flags.internal-interpreter = true;
  packages.ghci.flags.ghci = true;
  packages.ghci.flags.internal-interpreter = true;
}
