# This file is for configuration that should be required by every
# package set. Hopefully we can keep configuration for particular
# package sets out of this repo. Ideally, this file is only used for
# fixing things that are broken due to the Nix infrastructure.

{ pkgs, config, ... }:
let
  fromUntil = from: until: patch: { version }:
    if   builtins.compareVersions version from  >= 0
      && builtins.compareVersions version until <  0
      then patch
      else null;
  from = v: patch: { version }:
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

  # Avoid dependency on genprimopcode and deriveConstants (cabal does not put these in the plan,
  # most likely because it finds them in the PATH).
  # See https://github.com/input-output-hk/haskell.nix/issues/1808
  #
  # We now expose genprimopcode and deriveConstants from ghc directly (this is not in line with
  # with upstream ghc) to be able to re-build lib:ghc.
  packages.ghc.components.library.build-tools = pkgs.lib.mkForce (
    pkgs.lib.optionals (__compareVersions config.hsPkgs.ghc.identifier.version "9.4.1" > 0) [
      (config.hsPkgs.buildPackages.alex.components.exes.alex or pkgs.buildPackages.alex)
      (config.hsPkgs.buildPackages.happy.components.exes.happy or pkgs.buildPackages.happy)
    ]);

  # Remove dependency on hsc2hs (hsc2hs should be in ghc derivation)
  packages.mintty.components.library.build-tools = pkgs.lib.mkForce [];

  packages.ghc-lib-parser.patches = [
    (fromUntil "8.10.0.0" "9.2" ../overlays/patches/ghc-lib-parser-8.10-global-unique-counters-in-rts.patch)
    (fromUntil "9.2.0.0" "9.3" ../overlays/patches/ghc-lib-parser-9.2-global-unique-counters-in-rts.patch)
    (fromUntil "9.4.0.0" "9.7" ../overlays/patches/ghc-lib-parser-9.4-global-unique-counters-in-rts.patch)
  ];

  # See https://github.com/haskell-nix/hnix/pull/1053
  packages.hnix.patches = [
    (fromUntil "0.16.0" "0.16.0.1" ../patches/hnix.patch)
  ];

  # See https://github.com/input-output-hk/haskell.nix/issues/1455
  # This is a work around to make `ghcide` and `haskell-language-server` build with the unboxed tuple patch.
  packages.ghcide.patches =
    # Work out if we have applied the unboxed tupple patch in overlays/bootstrap.nix
    pkgs.lib.optionals (__elem config.compiler.nix-name [
      "ghc8101" "ghc8102" "ghc8103" "ghc8104" "ghc8105" "ghc8106" "ghc8107" "ghc810420210212"
    ]) [
      (fromUntil "1.7.0.0" "1.8.0.0" ../patches/ghcide-1.7-unboxed-tuple-fix-issue-1455.patch)
      (fromUntil "1.8.0.0" "1.11.0.0" ../patches/ghcide-1.8-unboxed-tuple-fix-issue-1455.patch)
    ]
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

  packages.llvm-hs.components.library.build-tools = pkgs.lib.mkForce [
    (fromUntil "5.0.0" "6" pkgs.llvmPackages_5.llvm)
    (fromUntil "6.0.0" "7" pkgs.llvmPackages_6.llvm)
    (fromUntil "7.0.0" "8" pkgs.llvmPackages_7.llvm)
    (fromUntil "8.0.0" "9" pkgs.llvmPackages_8.llvm)
    (fromUntil "9.0.0" "12" pkgs.llvmPackages_9.llvm)
    (fromUntil "12.0.0" "15" pkgs.llvmPackages_12.llvm)
    # NOTE: we currently don't have a llvm versoin > 12 that has a tag
    #       in nixpkgs, so we probably can't build `llvm-hs > 12`, there
    #       is however a head version of llvm in nixpkgs, which we might
    #       be able to use if that case were to occur
  ];

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

  packages.pcap.components.library.libs = [
    pkgs.libpcap
  ];

  # Build ghci and ghc with internal interpreter support to make the
  # `reinstallableLibGhc` build more like the boot version.
  # See https://github.com/input-output-hk/haskell.nix/issues/1512
  packages.ghc.flags.ghci = true;
  packages.ghc.flags.internal-interpreter = true;
  packages.ghci.flags.ghci = true;
  packages.ghci.flags.internal-interpreter = true;

  # See https://github.com/Bodigrim/bitvec/pull/61
  packages.bitvec.patches = [
    (fromUntil "1.1.3.0" "1.1.3.0.1" ../patches/bitvec-gmp-fix.patch)
  ];
}
