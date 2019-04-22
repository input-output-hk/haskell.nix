{ stdenv, buildPackages, ghc, lib, pkgconfig, writeText, runCommand
, haskellLib, nonReinstallablePkgs, withPackage, hsPkgs, setup-depends
, package, name, src, flags }:

let
  fullName = "${name}-setup";

  flagsAndConfig = field: xs: lib.optionalString (xs != []) ''
    echo ${lib.concatStringsSep " " (map (x: "--${field}=${x}") xs)} >> $out/configure-flags
    echo "${field}: ${lib.concatStringsSep " " xs}" >> $out/cabal.config
  '';

  flatDepends =
    let
      makePairs = map (p: rec { key="${val}"; val=(p.components.library or p); });
      closure = builtins.genericClosure {
        startSet = makePairs setup-depends;
        operator = {val,...}: makePairs val.config.depends;
      };
    in map ({val,...}: val) closure;

  configFiles = runCommand "${fullName}-config" { nativeBuildInputs = [ghc]; } (''
    mkdir -p $out

    # Calls ghc-pkg for the target platform
    target-pkg() {
      ${ghc.targetPrefix}ghc-pkg "$@"
    }

    target-pkg init $out/package.conf.d

    # Copy over the nonReinstallablePkgs from the global package db.
    # Note: we need to use --global-package-db with ghc-pkg to prevent it
    #       from looking into the implicit global package db when registering the package.
    ${lib.concatMapStringsSep "\n" (p: ''
      target-pkg describe ${p} | target-pkg --force --global-package-db $out/package.conf.d register - || true
    '') nonReinstallablePkgs}

    ${lib.concatMapStringsSep "\n" (p: ''
      target-pkg --package-db ${p}/package.conf.d dump | target-pkg --force --package-db $out/package.conf.d register -
    '') flatDepends}

    # Note: we pass `clear` first to ensure that we never consult the implicit global package db.
    ${flagsAndConfig "package-db" ["clear" "$out/package.conf.d"]}

    echo ${lib.concatStringsSep " " (lib.mapAttrsToList (fname: val: "--flags=${lib.optionalString (!val) "-" + fname}") flags)} >> $out/configure-flags

  ''
  # This code originates in the `generic-builder.nix` from nixpkgs.  However GHC has been fixed
  # to drop unused libraries referneced from libraries; and this patch is usually included in the
  # nixpkgs's GHC builds.  This doesn't sadly make this stupid hack unnecessary.  It resurfes in
  # the form of Cabal trying to be smart. Cabal when linking a library figures out that you likely
  # need those `rpath` entries, and passes `-optl-Wl,-rpath,...` for each dynamic library path to
  # GHC, thus subverting the linker and forcing it to insert all those RPATHs weather or not they
  # are needed.  We therfore reuse the linker hack here to move all al dynamic lirbaries into a
  # common folder (as links) and thus prevent Cabal from going nuts.
  #
  # TODO: Fix Cabal.
  # TODO: this is only needed if we do dynamic libraries.
  + lib.optionalString stdenv.isDarwin ''
    # Work around a limit in the macOS Sierra linker on the number of paths
    # referenced by any one dynamic library:
    #
    # Create a local directory with symlinks of the *.dylib (macOS shared
    # libraries) from all the dependencies.
    local dynamicLinksDir="$out/lib/links"
    mkdir -p $dynamicLinksDir
    for d in $(grep dynamic-library-dirs "$out/package.conf.d/"*|awk '{print $2}'|sort -u); do
      ln -s "$d/"*.dylib $dynamicLinksDir
    done
    # Edit the local package DB to reference the links directory.
    for f in "$out/package.conf.d/"*.conf; do
      sed -i "s,dynamic-library-dirs: .*,dynamic-library-dirs: $dynamicLinksDir," $f
    done
  '' + ''
    target-pkg --package-db $out/package.conf.d recache
  '' + ''
    target-pkg --package-db $out/package.conf.d check
  '');

in stdenv.lib.fix (drv:

stdenv.mkDerivation {
  name = "${fullName}";
  inherit src;
  nativeBuildInputs = [ghc];

  CABAL_CONFIG = configFiles + /cabal.config;
  phases = ["unpackPhase" "buildPhase" "installPhase"];
  buildPhase = ''
    for f in Setup.hs Setup.lhs; do
      if [ -f $f ]; then
        echo Compiling package $f
        ghc $f --make -o ./Setup
        setup=$(pwd)/Setup
      fi
    done
    [ -f ./Setup ] || (echo Failed to build Setup && exit 1)
  '';

  installPhase = ''
    mkdir -p $out/bin
    install ./Setup $out/bin/Setup
  '';
})


