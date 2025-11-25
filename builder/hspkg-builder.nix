{ pkgs, buildPackages, stdenv, lib, haskellLib, ghc, compiler-nix-name, fetchurl, runCommand, comp-builder, setup-builder }:

config:
{ flags
, package
, components
, cabal-generator

, name
, sha256
, src
, package-description-override
, patches

, shellHook

, ...
}@pkg:

let
  # Some packages bundled with GHC are not the same as they are in hackage.
  bundledSrc = {
      # These are problematic because the hackage versions will not install and are part of LTS.
      "ghc902/stm-2.5.0.0" = "/libraries/stm";
      "ghc902/filepath-1.4.2.1" = "/libraries/filepath";
    }."${compiler-nix-name}/${name}" or null;
  src =
    if bundledSrc != null
      then ghc.configured-src + bundledSrc
    else pkg.src;

  cabalFile = if package-description-override == null || bundledSrc != null then null else package-description-override;

  # New GHC JS backend run emcc itself without the need for custom Setup.hs
  oldGhcjs = stdenv.hostPlatform.isGhcjs && builtins.compareVersions ghc.version "9.10" < 0;
  defaultSetupSrc = if oldGhcjs then ./Setup.ghcjs.hs else ./Setup.hs;

  setup = if package.buildType == "Simple"
    then
      if oldGhcjs
        then
          buildPackages.haskell-nix.nix-tools-unchecked.exes.default-setup-ghcjs // { exeName = "default-setup-ghcjs"; }
        else
          buildPackages.haskell-nix.nix-tools-unchecked.exes.default-setup // { exeName = "default-setup"; }
    else setup-builder ({
      component = components.setup // {
        depends = config.setup-depends ++ components.setup.depends ++ package.setup-depends;
        extraSrcFiles = components.setup.extraSrcFiles ++ [ "Setup.hs" "Setup.lhs" ];
        pkgconfig = if components ? library then components.library.pkgconfig or [] else [];
      };
      inherit package name src flags patches defaultSetupSrc;
      inherit (pkg) preUnpack postUnpack prePatch postPatch;
    } // lib.optionalAttrs (package.buildType != "Custom") {
      nonReinstallablePkgs = ["base" "Cabal"];
    });

  buildComp = allComponent: componentId: component: comp-builder {
    inherit allComponent componentId component package name src flags setup cabalFile cabal-generator patches
            shellHook
            ;
    inherit (config) prebuilt-depends;
  };

in rec {
  components = haskellLib.applyComponents (buildComp pkg.allComponent) pkg;
  checks = pkgs.recurseIntoAttrs (builtins.mapAttrs
    (_: d: haskellLib.check d)
      (lib.filterAttrs (_: d: d.config.doCheck) components.tests));
  inherit (package) identifier detailLevel isLocal isProject buildType;
  inherit setup;
  isHaskell = true;
  inherit src;
}
