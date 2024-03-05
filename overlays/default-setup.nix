# This overlay adds the two versions of the default setup
# exe to the ghc derivations (one using the latest Cabal and
# one using the GHC provided Cabal).  These are then used
# when a package has the `Simple` build type.  Storing them
# on the GHC derivation means that nix eval does not have
# to compute the same derivation multiple times.
final: prev:
let
  nonReinstallablePkgs =
    if final.stdenv.targetPlatform.isGhcjs
      then ["base" "Cabal" "filepath" "directory"]
      else ["base" "Cabal"];
  haskellLib = final.haskell-nix.haskellLib;
  defaultSetupSrc =
    if final.stdenv.targetPlatform.isGhcjs
      then ../builder/Setup.ghcjs.hs
      else ../builder/Setup.hs;
  addDefaultSetup = compiler-nix-name: ghc:
    let
      # When building setup depends we need to use the build systems GHC and Packages
      makeSetupConfigFiles = haskellLib.weakCallPackage final.buildPackages ../builder/make-config-files.nix {
        inherit haskellLib nonReinstallablePkgs;
        ghc = (ghc.passthru.buildGHC or ghc);
      };
      setup-builder = haskellLib.weakCallPackage final ../builder/setup-builder.nix {
        ghc = (ghc.passthru.buildGHC or ghc);
        hsPkgs = {};
        # We need to use the buildPackages stdenv to build the setup-builder.
        # in the native case, it would be the same in the cross case however
        # we *really* want to build the Setup.hs on the build machine and not
        # have the stdenv confuse it with the target/host env.
        inherit (final.buildPackages) stdenv;
        inherit (final) buildPackages;
        inherit haskellLib nonReinstallablePkgs makeSetupConfigFiles;
      };

      # This is the `Cabal` library that was built for `cabal-install` to use.
      # It makes sense to use this version (when possible) because it will match the behavior of
      # building with `cabal-install` (including fixes that may not be in the
      # version of Cabal bundled with GHC).
      cabalFromCabalInstall = final.buildPackages.haskell-nix.cabal-install-unchecked.${compiler-nix-name}.project.hsPkgs.Cabal.components.library;

     in ghc // rec {
      defaultSetup = final.lib.mapAttrs (_: useCabalFromCabalInstall: setup-builder ({
        name = "${ghc.targetPrefix}default-Setup";
        component = {
          depends = final.lib.optional useCabalFromCabalInstall cabalFromCabalInstall;
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
          preUnpack = null;  postUnpack = null;
          prePatch = null;   postPatch = null;
          preBuild = null;   postBuild = null;
          preInstall = null; postInstall = null;
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
        cleanSrc = final.buildPackages.runCommand "default-Setup-src" {} ''
          mkdir $out
          cat ${defaultSetupSrc} > $out/Setup.hs
        '';
        inherit defaultSetupSrc;
      } // final.lib.optionalAttrs useCabalFromCabalInstall {
        # This is needed so that we don't get duplicate packages when we
        # add a custom Cabal package to the dependencies.  That way custom
        # setups won't complain about e.g. binary from the Cabal dependencies
        # and binary from the global package-db.
        nonReinstallablePkgs = ["base"];
      })) {
        useCabalFromCabalInstall = true;
        useCabalFromGHC = false;
      };

      # Check there is no chance we are building `cabalFromCabalInstall`.  Using `cabalFromCabalInstall`
      # to build itself would cause infinite recursion.
      defaultSetupFor = packageName:
        if
          # Cabal that comes with GHC 9.6.3 is newer than cabal-install
          __compareVersions ghc.version "9.6.3" < 0
          && (
            # `cabalFromCabalInstall` is not cross compiled
            final.stdenv.buildPlatform != final.stdenv.hostPlatform
          ||
            # These are the dependencies of `Cabal`
            !builtins.elem packageName
              ["alex" "happy" "hscolour" "Cabal" "Cabal-syntax" "bytestring" "time"
               "filepath" "base-compat-batteries" "base-compat" "unix" "directory" "transformers"
               "containers" "binary" "mtl" "text" "process" "parsec" "stm" "exceptions"]
          )
        then defaultSetup.useCabalFromCabalInstall
        else defaultSetup.useCabalFromGHC;
    };
in {
  haskell-nix = prev.haskell-nix // {
    compiler = final.lib.mapAttrs addDefaultSetup prev.haskell-nix.compiler;
    bootstrap = prev.haskell-nix.bootstrap // {
      compiler = final.lib.mapAttrs addDefaultSetup prev.haskell-nix.bootstrap.compiler;
    };
  };
  haskell = prev.haskell // {
    compiler = final.lib.mapAttrs addDefaultSetup prev.haskell.compiler;
  };
}
