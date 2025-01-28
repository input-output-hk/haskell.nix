{ lib, config, pkgs, haskellLib, ... }:
let
  inherit (config) name version revision;
in {
  _file = "haskell.nix/modules/hackage-project.nix";
  options = {
    version = lib.mkOption {
      type = lib.types.str;
      default = "latest";
      description = ''Version of the hackage package to use (defaults to "latest")'';
    };
  };
  config = {
    cabalProject = ''
      extra-packages: ${config.name}${lib.optionalString (config.version != "latest") "-${config.version}"}
    '';
    src = lib.mkDefault { outPath = pkgs.pkgsBuildBuild.runCommand "empty" {} "mkdir $out; touch $out/.not-completely-empty"; filterPath = { path, ... }: path; };
  };
}
