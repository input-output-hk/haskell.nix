{ lib, config, pkgs, haskellLib, ... }:
with lib;
with types;
{
  options = {
    # Used by callStackToNix
    name = mkOption {
      type = nullOr str;
      default = config.src.name or null;
      description = "Optional name for better error messages";
    };
    src = mkOption {
      type = either path package;
    };
    stackYaml = mkOption {
      type = str;
      default = "stack.yaml";
    };
    ignorePackageYaml = mkOption {
      type = bool;
      default = false;
    };
    cache = mkOption {
      type = nullOr unspecified;
      default = null;
    };
    stack-sha256 = mkOption {
      type = nullOr str;
      default = null;
    };
    resolverSha256 = mkOption {
      type = nullOr str;
      default = null;
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
    caller = mkOption {
      type = str;
      default = "callCabalProjectToNix";
      description = "Name of the calling function for better warning messages";
    };
    nix-tools = mkOption {
      type = nullOr package;
      default = pkgs.haskell-nix.internal-nix-tools;
      description = "nix-tools to use when converting the `plan.json` to nix";
    };

    # Used by mkStackPkgSet
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

    # Used in stack-cache-generator.nix
    sha256map = mkOption {
      type = nullOr unspecified;
      default = null;
      description = ''
        An alternative to adding `# nix-sha256:` comments into the
        stack.yaml file:
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
    branchMap = mkOption {
      type = nullOr unspecified;
      default = null;
      description = "A way to specify in which branch a git commit can be found";
    };
    lookupBranch = mkOption {
      type = nullOr unspecified;
      default = if config.branchMap != null
        then { location, tag, ...}: config.branchMap.${location}.${tag} or null
        else _: null;
    };

    # Used by stackProject itself
    compiler-nix-name = mkOption {
      type = nullOr str;
      description = "The name of the ghc compiler to use eg. \"ghc884\"";
      default = null;
    };
    ghc = mkOption {
      type = nullOr package;
      default = null;
      description = "Deprecated in favour of `compiler-nix-name`";
    };
  };
}
