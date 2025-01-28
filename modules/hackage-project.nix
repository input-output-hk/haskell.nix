{ lib, config, pkgs, haskellLib, ... }:
let
  inherit (config) name version;
  fullName = name + lib.optionalString (version != "latest") "-${version}";
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
    src = lib.mkDefault {
      outPath =
        pkgs.pkgsBuildBuild.runCommand "from-hackage-${fullName}" {} ''
          mkdir $out
          echo "extra-packages: ${fullName}" > $out/cabal.project
        '';
      # Disable git cleanSourceWith filtering
      filterPath = { path, ... }: path;
    };
  };
}
