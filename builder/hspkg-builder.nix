{ pkgs, buildPackages, stdenv, lib, haskellLib, ghc, compiler-nix-name, fetchurl, runCommand, comp-builder, setup-builder, inputMap }:

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
  baseUrlMatch = __match "(.*)/package/[^/]*" pkg.src.url;
  cabalFile = if revision == null || revision == 0 || bundledSrc != null then null else
    if pkgs.lib.hasPrefix "mirror://hackage/" pkg.src.url
      then fetchurl {
        name = "${name}-${toString revision}.cabal";
        url = "https://hackage.haskell.org/package/${name}/revision/${toString revision}.cabal";
        sha256 = revisionSha256;
      }
    else if baseUrlMatch != null
      then builtins.path {
        path = inputMap.${__head baseUrlMatch}
            or (throw "Unable to find inputMap entry for repository '${__head baseUrlMatch}' while looking for '${package.identifier.name}' revision.")
          + "/index/${package.identifier.name}/${package.identifier.version}/${package.identifier.name}.cabal";
        sha256 = revisionSha256;
        recursive = false;
      }
    else throw "Unexpected url '${pkg.src.url}' format while looking for '${package.identifier.name}' revision";

  defaultSetupSrc = if stdenv.hostPlatform.isGhcjs then ./Setup.ghcjs.hs else ./Setup.hs;

  # This is the `Cabal` library that was built for `cabal-install` to use.
  # It makes sense to use this version (when possible) because it will match the behavior of
  # building with `cabal-install` (including fixes that may not be in the
  # version of Cabal bundled with GHC).
  cabalFromCabalInstall = buildPackages.haskell-nix.cabal-install-unchecked.${compiler-nix-name}.project.hsPkgs.Cabal.components.library;

  # Check there is no chance we are building `cabalFromCabalInstall`.  Using `cabalFromCabalInstall`
  # to build itseld would cause infinite recursion.
  useCabalFromCabalInstall =
        # `cabalFromCabalInstall` is not cross compiled
        stdenv.buildPlatform != stdenv.hostPlatform
      ||
        # These are the dependencies of `Cabal`
        !builtins.elem package.identifier.name
          ["nix-tools" "alex" "happy" "hscolour" "Cabal" "bytestring" "aeson" "time"
           "filepath" "base-compat-batteries" "base-compat" "unix" "directory" "transformers"
           "containers" "binary" "mtl" "text" "process" "parsec"];

  defaultSetup = setup-builder ({
    name = "${ghc.targetPrefix}default-Setup";
    component = {
      depends = config.setup-depends ++ lib.optional useCabalFromCabalInstall cabalFromCabalInstall;
      libs = [];
      frameworks = [];
      doExactConfig = false;
      includeDirs = [];
      asmSources = [];
      cSources = [];
      cmmSources = [];
      cxxSources = [];
      jsSources = [];
      extraSrcFiles = [ "Setup.hs" "Setup.lhs" ];
      pkgconfig = [];
      build-tools = [];

      platforms = null;
      preBuild = null;   postBuild = null;
      preInstall = null; postInstall = null;
      preUnpack = null;  postUnpack = null;
    };
    package = {
      identifier = {
        name = "default-Setup";
        version = "1.0";
      };
      homepage = null;
      synopsis = null;
      license = "MIT";
    };
    src = null;
    cleanSrc = buildPackages.runCommand "default-Setup-src" {} ''
      mkdir $out
      cat ${defaultSetupSrc} > $out/Setup.hs
    '';
    inherit defaultSetupSrc;
  } // lib.optionalAttrs useCabalFromCabalInstall {
    # This is needed so that we don't get duplicate packages when we
    # add a custom Cabal package to the dependencies.  That way custom
    # setups won't complain about e.g. binary from the Cabal dependencies
    # and binary from the global package-db.
    nonReinstallablePkgs = [];
  });

  # buildPackages.runCommand "default-Setup" { nativeBuildInputs = [(ghc.passthru.buildGHC or ghc)]; } ''
  #   cat ${defaultSetupSrc} > Setup.hs
  #   mkdir -p $out/bin
  #   ${(ghc.passthru.buildGHC or ghc).targetPrefix}ghc Setup.hs --make -o $out/bin/Setup
  # '';

  setup = if package.buildType == "Simple"
    then defaultSetup
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
