{ pkgs, buildPackages, stdenv, lib, haskellLib, ghc, compiler-nix-name, fetchurl, runCommand, comp-builder, setup-builder }:

config:
{ flags
, package
, components
, cabal-generator

, name
, sha256
, src
, revision
, revisionSha256
, patches

, shellHook

, ...
}@pkg:

assert (if ghc.isHaskellNixCompiler or false then true
  else throw ("It is likely you used `haskell.compiler.X` instead of `haskell-nix.compiler.X`"
    + pkgs.lib.optionalString (name != null) (" for " + name)));

let
  # Some packages bundled with GHC are not the same as they are in hackage.
  bundledSrc = {
      # These are problematic because the hackage versions will not install and are part of LTS.
      "ghc902/stm-2.5.0.0" = "/libraries/stm";
      "ghc902/filepath-1.4.2.1" = "/libraries/filepath";
    }."${compiler-nix-name}/${name}" or null;
  src = if bundledSrc == null then pkg.src else ghc.configured-src + bundledSrc;
  cabalFile = if revision == null || revision == 0 || bundledSrc != null then null else
    fetchurl {
      name = "${name}-${toString revision}.cabal";
      url = "https://hackage.haskell.org/package/${name}/revision/${toString revision}.cabal";
      sha256 = revisionSha256;
    };

  defaultSetupSrc = if stdenv.hostPlatform.isGhcjs then ./Setup.ghcjs.hs else ./Setup.hs;

  setup = if package.buildType == "Simple"
    then ghc.defaultSetupFor package.identifier.name
    else setup-builder {
      component = components.setup // {
        depends = config.setup-depends ++ components.setup.depends ++ package.setup-depends;
        extraSrcFiles = components.setup.extraSrcFiles ++ [ "Setup.hs" "Setup.lhs" ];
        pkgconfig = if components ? library then components.library.pkgconfig or [] else [];
      };
      inherit package name src flags revision patches defaultSetupSrc;
      inherit (pkg) preUnpack postUnpack;
    };

  buildComp = allComponent: componentId: component: comp-builder {
    inherit allComponent componentId component package name src flags setup cabalFile cabal-generator patches revision
            shellHook
            ;
  };

in rec {
  components = haskellLib.applyComponents (buildComp pkg.allComponent) pkg;
  checks = pkgs.recurseIntoAttrs (builtins.mapAttrs
    (_: d: haskellLib.check d)
      (lib.filterAttrs (_: d: d.config.doCheck) components.tests));
  inherit (package) identifier detailLevel isLocal isProject buildType;
  inherit setup cabalFile;
  isHaskell = true;
  inherit src;
}
