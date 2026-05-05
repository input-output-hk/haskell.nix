# While creating the nix build plan, we take care to create package derivations
# that do not include any reference to the plan itself or how it is created.
#
# This allows haskell.nix to share packages between plans (at least when they
# have identical dependencies). If the package derivations included the hash of
# the plan derivation, different plans would always produce different packages
# and there could not be any sharing of packages between plans.
#
# Any wrangling of the project dependencies (e.g. fetching package indices,
# source-repository-packages or any other asset required for building) *has* to
# be performed during planning. The nix build plan will import any remote asset
# through a fixed-output derivations (i.e. a call to a fetcher).
#
# tl;dr: the builder must not re-introduce any reference to the build plan.

{ pkgs, buildPackages, pkgsBuildBuild, evalPackages, stdenv, lib, haskellLib, ghc, compiler-nix-name, fetchurl, nonReinstallablePkgs, hsPkgs, compiler, builderVersion ? 1, crossTemplateHaskellSupport ? true }:

let
  # Builds a single component of a package.
  comp-builder = haskellLib.weakCallPackage pkgs ./comp-builder.nix {
    inherit ghc haskellLib makeConfigFiles haddockBuilder ghcForComponent hsPkgs compiler nonReinstallablePkgs;
  };

  # Alternative component builder that runs cabal v2-build and emits
  # a cabal-store slice rather than going through Setup.hs.  Selected
  # when the project sets `builderVersion = 2` (the default).  See
  # builder/comp-v2-builder.nix for the per-kind behaviour.
  #
  # Build cabal-install via `haskell-nix.tool`.
  #
  # Two pins here:
  #
  #   * `builderVersion = 1` — cabal-install stays a v1 derivation.
  #     Every v2 slice lists v2CabalInstall as a nativeBuildInput, so
  #     producing v2CabalInstall as a v2 slice would cycle on itself.
  #
  #   * `compilerSelection = p: p.haskell.compiler` — use nixpkgs's
  #     pre-built GHC rather than haskell.nix's own (which would be
  #     built by hadrian, itself a haskell.nix project whose hackage
  #     deps become v2 slices needing v2CabalInstall).  Pre-built
  #     nixpkgs GHC sidesteps the entire hadrian subtree.
  #
  # We go through `tool` (rather than nix-tools' pre-built cabal) so
  # we can apply our own cabal-install patches — notably
  # prune-unreachable-sublibs.patch, which lets the solver ignore
  # public sublibs whose deps aren't in plan.nix (e.g. vector's
  # `benchmarks-O2` sublib needing tasty).
  v2CabalInstall = pkgsBuildBuild.haskell-nix.tool "ghc9141" "cabal" {
    version = "3.16.1.0";
    builderVersion = 1;
    compilerSelection = p: p.haskell.compiler;
    modules = [{
      packages.cabal-install-solver.patches = [
        ./cabal-install-patches/prune-unreachable-sublibs.patch
      ];
      packages.cabal-install.patches = [
        ./cabal-install-patches/prune-unreachable-sublibs-installplan.patch
        ./cabal-install-patches/skip-installed-revdeps-in-completed.patch
      ];
    }];
  };

  buildCabalStoreSlice = import ./build-cabal-slice.nix {
    inherit stdenv lib ghc pkgsBuildBuild buildPackages;
    cabal-install = v2CabalInstall;
  };

  # Helper that composes a list of v2 slices into one cabal-store
  # layout.  Used by `comp-v2-builder` to attach a `.store`
  # derivation to each slice (containing the component's
  # *dependencies* — not the component itself), and by the v2 shell
  # to compose the user-facing dep store.
  composeStore = import ./compose-store.nix {
    inherit lib ghc;
    inherit (pkgs) runCommand;
    lndir = pkgs.buildPackages.lndir or pkgs.buildPackages.xorg.lndir;
  };
  # If the host platform has cross-TH plumbing (currently only
  # windows via overlays/windows.nix) it lives at
  # `pkgs.haskell-nix.templateHaskell.<compiler-nix-name>`.  Pull
  # it out here so comp-v2-builder doesn't need its own
  # compiler-nix-name lookup just to wrap the ghc we already have.
  # Honour the project-level `crossTemplateHaskellSupport` switch
  # so projects that the wrapper itself depends on (iserv-proxy)
  # can opt out, breaking the otherwise-circular dep on the
  # wrapped ghc's inputs.
  templateHaskell =
    if !crossTemplateHaskellSupport then null
    else pkgs.haskell-nix.templateHaskell.${compiler-nix-name} or null;

  comp-v2-builder = haskellLib.weakCallPackage pkgs ./comp-v2-builder.nix {
    inherit ghc hsPkgs buildCabalStoreSlice templateHaskell haskellLib composeStore;
  };

  haddockBuilder = haskellLib.weakCallPackage pkgs ./haddock-builder.nix {
    inherit ghc ghcForComponent haskellLib makeConfigFiles nonReinstallablePkgs;
  };

  setup-builder = haskellLib.weakCallPackage pkgs ./setup-builder.nix {
    ghc = (ghc.buildGHC or ghc);
    hsPkgs = hsPkgs.buildPackages;
    # We need to use the buildPackages stdenv to build the setup-builder.
    # in the native case, it would be the same in the cross case however
    # we *really* want to build the Setup.hs on the build machine and not
    # have the stdenv confuse it with the target/host env.
    inherit (buildPackages) stdenv;
    inherit buildPackages;
    inherit haskellLib nonReinstallablePkgs makeSetupConfigFiles;
  };

  # Wraps GHC to provide dependencies in a way that works for both the
  # component builder and for nix-shells.
  ghcForComponent = import ./ghc-for-component-wrapper.nix {
    inherit lib ghc haskellLib;
    inherit (pkgs) stdenv;
    inherit (buildPackages.buildPackages) runCommand makeWrapper;
    lndir = buildPackages.buildPackages.lndir or buildPackages.buildPackages.xorg.lndir;
  };

  # Builds a derivation which contains a ghc package-db of
  # dependencies for a component.
  makeConfigFiles = haskellLib.weakCallPackage pkgs ./make-config-files.nix {
    inherit ghc haskellLib nonReinstallablePkgs;
  };
  # When building setup depends we need to use the build systems GHC and Packages
  makeSetupConfigFiles = haskellLib.weakCallPackage buildPackages ./make-config-files.nix {
    inherit haskellLib nonReinstallablePkgs;
    ghc = (ghc.buildGHC or ghc);
  };


  hoogleLocal = let
    # Use hoogle.nix from at least nixpkgs 22.05
    nixpkgs = if lib.versionAtLeast lib.trivial.release "22.05"
      then pkgs.path
      else pkgs.haskell-nix.sources.nixpkgs-2205;
    nixpkgsHoogle = import (nixpkgs + /pkgs/development/haskell-modules/hoogle.nix);
  in { packages ? [], hoogle }:
    let
      haskellPackages = {
        # For musl we can use haddock from the buildGHC
        ghc = if stdenv.targetPlatform.isMusl
          then ghc.buildGHC
          else ghc;
        inherit packages hoogle;
      };
    in haskellLib.weakCallPackage pkgs nixpkgsHoogle {
      inherit haskellPackages;
    } (p: p.packages);

  # `shellFor` dispatches on the project's `builderVersion`.  The
  # primary derivation per component is whichever builder
  # `builderVersion` selects, so the shell has to match.  See
  # `builder/shell-for-v2.nix` for the v2 semantics; v2 is the only
  # shell that supports public sublibs (`library <n>`).
  shellForV1 = haskellLib.weakCallPackage pkgs ./shell-for.nix {
    inherit hsPkgs ghcForComponent makeConfigFiles hoogleLocal haskellLib pkgsBuildBuild evalPackages compiler ghc;
    inherit (buildPackages) glibcLocales llvmPackages;
  };
  shellForV2 = haskellLib.weakCallPackage pkgs ./shell-for-v2.nix {
    inherit hsPkgs haskellLib ghc compiler composeStore;
    haskell-nix = pkgs.haskell-nix;
  };
  shellFor = if builderVersion == 2 then shellForV2 else shellForV1;

  # Same as haskellPackages.ghcWithPackages and ghcWithHoogle in nixpkgs.
  withPackages = {withHoogle}: packages: (shellForV1 {
    name = ghc.name + "-with-packages";
    packages = _: [];
    additional = packages;
    inherit withHoogle;
  }).ghc;

in {
  # Build a Haskell package from its config.
  # TODO: this pkgs is the adjusted pkgs, but pkgs.pkgs is unadjusted
  build-package = haskellLib.weakCallPackage pkgs ./hspkg-builder.nix {
    inherit haskellLib ghc compiler-nix-name comp-builder comp-v2-builder setup-builder builderVersion;
  };

  inherit shellFor makeConfigFiles;

  ghcWithPackages = withPackages { withHoogle = false; };
  ghcWithHoogle = withPackages { withHoogle = true; };
}
