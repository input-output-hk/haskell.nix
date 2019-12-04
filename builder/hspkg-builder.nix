{ pkgs, buildPackages, stdenv, lib, haskellLib, ghc, fetchurl, runCommand, comp-builder, setup-builder }:


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
}@config:

assert (if ghc.isHaskellNixCompiler or false then true
  else throw ("It is likely you used `haskell.compiler.X` instead of `haskell-nix.compiler.X`"
    + pkgs.lib.optionalString (name != null) (" for " + name)));

let
  cabalFile = if revision == null || revision == 0 then null else
    fetchurl {
      name = "${name}-${toString revision}.cabal";
      url = "https://hackage.haskell.org/package/${name}/revision/${toString revision}.cabal";
      sha256 = revisionSha256;
    };

  defaultSetupSrc = builtins.toFile "Setup.hs" ''
    import Distribution.Simple
    main = defaultMain
  '';
  defaultSetup = buildPackages.runCommand "default-Setup" { nativeBuildInputs = [(ghc.passthru.buildGHC or ghc)]; } ''
    cat ${defaultSetupSrc} > Setup.hs
    mkdir -p $out/bin
    ${(ghc.passthru.buildGHC or ghc).targetPrefix}ghc Setup.hs --make -o $out/bin/Setup
  '';

  setup = if package.buildType == "Simple" && package.setup-depends == []
    then defaultSetup
    else setup-builder {
      component = components.setup // {
        depends = components.setup.depends ++ package.setup-depends;
        extraSrcFiles = components.setup.extraSrcFiles ++ [ "Setup.hs" "Setup.lhs" ];
      };
      inherit package name src flags revision patches defaultSetupSrc;
      inherit (config) preUnpack postUnpack;
    };

  buildComp = componentId: component: comp-builder {
    inherit componentId component package name src flags setup cabalFile cabal-generator patches revision
            shellHook
            ;
  };

in rec {
  components = haskellLib.applyComponents buildComp config;
  checks = pkgs.recurseIntoAttrs (builtins.mapAttrs
    (_: d: haskellLib.check d)
      (lib.filterAttrs (_: d: d.config.doCheck) components.tests));
  inherit (package) identifier detailLevel isLocal;
  inherit setup cabalFile;
  isHaskell = true;
}
