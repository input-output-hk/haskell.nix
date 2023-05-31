final: prev:
let
  callCabal2Nix = compiler-nix-name: name: src: final.buildPackages.stdenv.mkDerivation {
    name = "${name}-package.nix";
    inherit src;
    nativeBuildInputs = [
      # It is not safe to check the nix-tools materialization here
      # as we would need to run this code to do so leading to
      # infinite recursion (so using nix-tools-unchecked).
      final.buildPackages.haskell-nix.nix-tools-unchecked.${compiler-nix-name}
    ];
    phases = [ "unpackPhase" "buildPhase" ];

    LOCALE_ARCHIVE = final.lib.optionalString (final.stdenv.hostPlatform.libc == "glibc") "${final.glibcLocales}/lib/locale/locale-archive";
    LANG = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";

    buildPhase = ''
      sed -i 's/^cabal-version: *2\.1/cabal-version: 3.0/' *.cabal
      cabal-to-nix *.cabal > $out
    '';
  };

  # Combines multiple derivations into one to make them
  # easier to materialize.
  # Using `cp -Lr` here follows the symlinks and prevents
  # `access to path is forbidden in restricted mode`
  # errors on hydra when the materialized files are not present.
  combineFiles = name: ext: files:
    let links = final.linkFarm name
      (final.lib.mapAttrsToList (name: path: {
        name = name + ext;
        inherit path;
      }) files);
    in final.buildPackages.runCommand "${name}${ext}" {} ''
      cp -Lr ${links} $out
      chmod -R +w $out
    '';

  # Combine the all the boot package nix files for a given ghc
  # into a single derivation and materialize it.
  combineAndMaterialize = unchecked: materialized-dir: ghcName: bootPackages:
      (final.haskell-nix.materialize ({
          materialized = materialized-dir + "/ghc-boot-packages-nix/${ghcName +
              # The 3434.patch we apply to fix linking on arm systems changes ghc-prim.cabal
              # so it needs its own materialization.
              final.lib.optionalString final.stdenv.targetPlatform.isAarch64 "-aarch64"
              # GHCJS bytestring and libiserv versions differs
              + final.lib.optionalString final.stdenv.hostPlatform.isGhcjs "-ghcjs"
            }";
        } // final.lib.optionalAttrs unchecked {
          checkMaterialization = false;
        }) (combineFiles "${ghcName}-boot-packages-nix" ".nix" (builtins.mapAttrs
          (_: srcAndNix: srcAndNix.nix) bootPackages)));

  # Import the nix and src.
  importSrcAndNix = srcAndNix:
      args: (import srcAndNix.nix args) // { inherit (srcAndNix) src; };

  # The packages in GHC source and the locations of them
  ghc-extra-pkgs = ghcVersion: {
      base         = "libraries/base";
      bytestring   = "libraries/bytestring";
      ghci         = "libraries/ghci";
      ghc-heap     = "libraries/ghc-heap";
      ghc-prim     = "libraries/ghc-prim";
      hpc          = "libraries/hpc";
      integer-gmp  = "libraries/integer-gmp";
      template-haskell = "libraries/template-haskell";
      iserv        = "utils/iserv";
    } // final.lib.optionalAttrs (!final.stdenv.hostPlatform.isGhcjs || builtins.compareVersions ghcVersion "9.6" < 0) {
      libiserv     = "libraries/libiserv";
    } // final.lib.optionalAttrs (builtins.compareVersions ghcVersion "9.6" > 0) {
      Cabal        = "libraries/Cabal/Cabal";
      Cabal-syntax = "libraries/Cabal/Cabal-syntax";
      cabal-install = "libraries/Cabal/cabal-install";
      cabal-install-solver = "libraries/Cabal/cabal-install-solver";
    } // final.lib.optionalAttrs (!final.stdenv.hostPlatform.isGhcjs) {
      ghc          = "compiler";
      ghc-boot     = "libraries/ghc-boot";
    } // (
      if builtins.compareVersions ghcVersion "9.4" < 0
        then {
          # The version of `Win32` that comes with ghc 9.4 (2.12.0.0) is older
          # than the one in hackage.  Including it causes `cabal configure` to fail.
          Win32        = "libraries/Win32";
          # As of GHC 9.4 this has been split out of the GHC repo and
          # is now in the iserv-proxy flake input
          iserv-proxy  = "utils/iserv-proxy";
        }
        else {
          genprimopcode = "utils/genprimopcode";
          deriveConstants = "utils/deriveConstants";
        }
    ) // final.lib.optionalAttrs (!final.stdenv.hostPlatform.isGhcjs || builtins.compareVersions ghcVersion "8.10.5" >= 0) {
      # Not sure why, but this is missing from older ghcjs versions
      remote-iserv = "utils/remote-iserv";
    } // final.lib.optionalAttrs (builtins.compareVersions ghcVersion "9.0.1" >= 0) {
      ghc-bignum   = "libraries/ghc-bignum";
    } // final.lib.optionalAttrs (builtins.compareVersions ghcVersion "9.2.1" >= 0) {
      deepseq      = "libraries/deepseq";
      pretty       = "libraries/pretty";
    };

  # The nix produced by `cabalProject` differs slightly depending on
  # what the platforms are.  There are currently 3 possible outputs.
  ghc-extra-projects-type = ghc:
    if final.stdenv.hostPlatform.isWindows
      then "windows"
      else if final.stdenv.hostPlatform.isGhcjs
        then "ghcjs"
        else if final.haskell-nix.haskellLib.isCrossHost
          then "cross"
          else "default";

# Given the ghc-extra-pkgs, we'll create a cabal.project
# that contains all of them.  And then we call cabalProject
# on it to generate the necessary cabal project exposing all
# the package components.
#
# The motivation here is that we can build primarily
# remote-iserv and iserv-proxy as standalone applications, as
# derived from the configured (and potentially patched) ghc
# source code.
#
# This is a bit like how we treat hsc2hs, alex, happy as external
# programs we need to build from hackage, but iserv-remote and
# iserv-proxy are not on hackage (and might have been patched)
# as part of patches we applied to the GHC tree.

in rec {
  inherit combineAndMaterialize;
  ghc-boot-packages-src-and-nix = builtins.mapAttrs
    (ghcName: ghc: builtins.mapAttrs
      (pkgName: subDir: rec {
        src =
          # TODO remove once nix >=2.4 is widely adopted (will trigger rebuilds of everything).
          # See https://github.com/input-output-hk/haskell.nix/issues/1459
          let nix24srcFix = src: src // { filterPath = { path, ... }: path; };
          # Add in the generated files needed by ghc-boot
          in if subDir == "libraries/ghc-boot"
            then nix24srcFix (final.buildPackages.runCommand "ghc-boot-src" { nativeBuildInputs = [final.buildPackages.xorg.lndir]; } ''
              mkdir $out
              lndir -silent ${ghc.passthru.configured-src}/${subDir} $out
              lndir -silent ${ghc.generated}/libraries/ghc-boot/dist-install/build/GHC $out/GHC
            '')
          else if subDir == "compiler"
            then final.haskell-nix.haskellLib.cleanSourceWith {
              src = nix24srcFix (final.buildPackages.runCommand "ghc-src" { nativeBuildInputs = [final.buildPackages.xorg.lndir]; } ''
                mkdir $out
                lndir -silent ${ghc.passthru.configured-src} $out
                if [[ -f ${ghc.generated}/libraries/ghc-boot/dist-install/build/GHC/Version.hs ]]; then
                  ln -s ${ghc.generated}/libraries/ghc-boot/dist-install/build/GHC/Version.hs $out/libraries/ghc-boot/GHC
                fi
                if [[ -f ${ghc.generated}/libraries/ghc-boot/dist-install/build/GHC/Platform/Host.hs ]]; then
                  ln -s ${ghc.generated}/libraries/ghc-boot/dist-install/build/GHC/Platform/Host.hs $out/libraries/ghc-boot/GHC/Platform
                fi
                if [[ -f ${ghc.generated}/compiler/stage2/build/Config.hs ]]; then
                  ln -s ${ghc.generated}/compiler/stage2/build/Config.hs $out/compiler
                fi
                if [[ -f ${ghc.generated}/compiler/stage2/build/GHC/Platform/Constants.hs ]]; then
                  ln -s ${ghc.generated}/compiler/stage2/build/GHC/Platform/Constants.hs $out/compiler/GHC/Platform
                fi
                if [[ -f ${ghc.generated}/compiler/stage2/build/GHC/Settings/Config.hs ]]; then
                  ln -s ${ghc.generated}/compiler/stage2/build/GHC/Settings/Config.hs $out/compiler/GHC/Settings
                fi
                ln -s ${ghc.generated}/includes/dist-derivedconstants/header/* $out/compiler
                ln -s ${ghc.generated}/compiler/stage2/build/*.hs-incl $out/compiler
              '');
              inherit subDir;
              includeSiblings = true;
            }
            else "${ghc.passthru.configured-src}/${subDir}";
        nix = callCabal2Nix ghcName "${ghcName}-${pkgName}" src;
      }) (ghc-extra-pkgs ghc.version))
    final.buildPackages.haskell-nix.compiler;

  # All the ghc boot package nix files for each ghc.
  ghc-boot-packages-nix = builtins.mapAttrs
    (combineAndMaterialize false ../materialized)
      ghc-boot-packages-src-and-nix;

  ghc-boot-packages-nix-unchecked = builtins.mapAttrs
    (combineAndMaterialize true ../materialized)
      ghc-boot-packages-src-and-nix;

  # The import nix results for each ghc boot package for each ghc.
  ghc-boot-packages = builtins.mapAttrs
    (ghcName: value: builtins.mapAttrs
      (pkgName: srcAndNix: importSrcAndNix {
        inherit (srcAndNix) src;
        nix = final.ghc-boot-packages-nix.${ghcName} + "/${pkgName}.nix";
      }) value)
        ghc-boot-packages-src-and-nix;

  ghc-boot-packages-unchecked = builtins.mapAttrs
    (ghcName: value: builtins.mapAttrs
      (pkgName: srcAndNix: importSrcAndNix {
        inherit (srcAndNix) src;
        nix = final.ghc-boot-packages-nix-unchecked.${ghcName} + "/${pkgName}.nix";
      }) value)
        ghc-boot-packages-src-and-nix;

  # Derivation with cabal.project for use with `cabalProject'` for each ghc.
  ghc-extra-pkgs-cabal-projects = builtins.mapAttrs (ghcName: ghc:
    let package-locs =
        # TODO ghc-heap.cabal requires cabal 3.  We should update the cabalProject' call
        # in `ghc-extra-projects` below to work with this.
        (final.lib.filterAttrs (n: _: !(builtins.elem n [ "base" "ghc-heap" "ghc-bignum" "ghc-prim" "integer-gmp" "template-haskell" "pretty" "bytestring" "deepseq"
           "Cabal" "Cabal-syntax" "cabal-install" "cabal-install-solver" ])) (ghc-extra-pkgs ghc.version));
      cabalProject = ''
        packages: ${final.lib.concatStringsSep " " (final.lib.attrValues package-locs)}
        allow-newer: iserv-proxy:bytestring, network:bytestring, iserv-proxy:containers
        -- need this for libiserv as it doesn't build against 3.0 yet.
        constraints: network < 3.0,
                     ghc +ghci,
                     ghci +ghci,
                     ghci +internal-interpreter,
                     libiserv +network
      '';
    in (final.stdenv.mkDerivation {
      name = "ghc-extra-pkgs-cabal-project-${ghcName}";
      phases = [ "buildPhase" ];
      # Copy each cabal file from the configured ghc source and
      # add a suitable cabal.project file.
      buildPhase = ''
        ${final.lib.concatStrings (final.lib.mapAttrsToList (_: dir: ''
          mkdir -p $out/${dir}
          cp ${ghc.passthru.configured-src}/${dir}/*.cabal $out/${dir}
          # Remove references to libffi as the are not cross platform
          # and will break memoization (we will need to add them back)
          sed -i 's|/nix/store/.*-libffi.*/include||' $out/${dir}/*.cabal
        '') package-locs)}
      '';
    }) // { inherit cabalProject ghc; }) final.buildPackages.haskell-nix.compiler;

  # A `cabalProject'` project for each ghc
  ghc-extra-projects = builtins.mapAttrs (ghcName: proj:
    final.haskell-nix.cabalProject' ({pkgs, ...}: {
      evalPackages = pkgs.buildPackages;
      name = "ghc-extra-projects-${ghc-extra-projects-type proj.ghc}-${ghcName}";
      src = proj;
      inherit (proj) cabalProject;
      # Avoid readDir and readFile IFD functions looking for these files
      cabalProjectLocal = null;
      cabalProjectFreeze = null;
      index-state = final.haskell-nix.internalHackageIndexState;
      # Where to look for materialization files
      materialized = ../materialized/ghc-extra-projects
                       + "/${ghc-extra-projects-type proj.ghc}/${ghcName}";
      compiler-nix-name = ghcName;
      configureArgs = "--disable-tests --disable-benchmarks --allow-newer='terminfo:base'"; # avoid failures satisfying bytestring package tests dependencies
      modules = [{
        packages.iserv-proxy.patches = [./patches/ghc/ghc-8.10.7-iserv-proxy-load-dlls.patch];
        reinstallableLibGhc = false;
      }];
    }))
    ghc-extra-pkgs-cabal-projects;

  # The packages from the project for each ghc
  ghc-extra-packages = builtins.mapAttrs (_: proj: proj.hsPkgs) ghc-extra-projects;
}
