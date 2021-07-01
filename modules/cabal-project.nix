{ lib, config, pkgs, haskellLib, ... }:
with lib;
with types;
let readIfExists = src: fileName:
      let origSrcDir = src.origSrcSubDir or src;
      in
        if builtins.elem ((__readDir origSrcDir)."${fileName}" or "") ["regular" "symlink"]
          then __readFile (origSrcDir + "/${fileName}")
          else null;
in {
  _file = "haskell.nix/modules/cabal-project.nix";
  options = {
    # Used by callCabalProjectToNix
    compiler-nix-name = mkOption {
      type = str;
      description = "The name of the ghc compiler to use eg. \"ghc884\"";
    };
    index-state = mkOption {
      type = nullOr str;
      default = null;
      description = "Hackage index-state, eg. \"2019-10-10T00:00:00Z\"";
    };
    index-sha256 = mkOption {
      type = nullOr str;
      default = null;
      description = "The hash of the truncated hackage index-state";
    };
    plan-sha256 = mkOption {
      type = nullOr str;
      default = null;
      description = "The hash of the plan-to-nix output (makes the plan-to-nix step a fixed output derivation)";
    };
    materialized = mkOption {
      type = nullOr (either path package);
      default = null;
      description = "Location of a materialized copy of the nix files";
    };
    checkMaterialization = mkOption {
      type = nullOr bool;
      default = null;
      description = "If true the nix files will be generated used to check plan-sha256 and material";
    };
    cabalProjectFileName = mkOption {
      type = str;
      default = "cabal.project";
    };
    cabalProject = mkOption {
      type = nullOr str;
      default = readIfExists config.src config.cabalProjectFileName;
    };
    cabalProjectLocal = mkOption {
      type = nullOr str;
      default = readIfExists config.src "${config.cabalProjectFileName}.local";
    };
    cabalProjectFreeze = mkOption {
      type = nullOr str;
      default = readIfExists config.src "${config.cabalProjectFileName}.freeze";
    };
    caller = mkOption {
      type = str;
      default = "callCabalProjectToNix";
      description = "Name of the calling function for better warning messages";
    };
    ghc = mkOption {
      type = nullOr package;
      default = null;
      description = "Deprecated in favour of `compiler-nix-name`";
    };
    ghcOverride = mkOption {
      type = nullOr package;
      default = null;
      description = "Used when we need to set ghc explicitly during bootstrapping";
    };
    # The defaults for `nix-tools` and `cabal-install` are in `call-cabal-project-to-nix.nix`
    # to make sure it is not evaluated too strictly (which would lead to infinite recursion).
    nix-tools = mkOption {
      type = nullOr package;
      default = null;
      description = "nix-tools to use when converting the `plan.json` to nix";
    };
    cabal-install = mkOption {
      type = nullOr package;
      default = null;
      description = "cabal-install to use when running `cabal configure`";
    };
    configureArgs = mkOption {
      type = nullOr str;
      default = "";
      description = ''
        Extra arguments to pass to `cabal v2-configure`.
        `--enable-tests --enable-benchmarks` are included by default.
        If the tests and benchmarks are not needed and they
        cause the wrong plan to be chosen, then we can use
        `configureArgs = "--disable-tests --disable-benchmarks";`
      '';
    };
    sha256map = mkOption {
      type = nullOr unspecified;
      default = null;
      description = ''
        An alternative to adding `--sha256` comments into the
        cabal.project file:
          sha256map =
            { "https://github.com/jgm/pandoc-citeproc"."0.17"
              = "0dxx8cp2xndpw3jwiawch2dkrkp15mil7pyx7dvd810pwc22pm2q"; };
      '';
    };
    lookupSha256 = mkOption {
      type = nullOr unspecified;
      default = if config.sha256map != null
        then { location, tag, ...}: config.sha256map.${location}.${tag}
        else _: null;
    };
    extra-hackage-tarballs = mkOption {
      type = nullOr (listOf unspecified);
      default = [];
    };

    # Used by mkCabalProjectPkgSet
    pkg-def-extras = mkOption {
      type = nullOr (listOf unspecified);
      default = [];
    };
    modules = mkOption {
      type = nullOr (listOf unspecified);
      default = [];
    };
    extra-hackages = mkOption {
      type = nullOr (listOf unspecified);
      default = [];
    };
  };
}
