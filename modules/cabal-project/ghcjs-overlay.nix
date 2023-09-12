{ lib, config, pkgs, ... }:
let
  cfg = config.ghcjs-overlay;
in
{
  _file = "haskell.nix/modules/cabal-project/ghcjs-overlay.nix";

  options = {
    ghcjs-overlay.enable = lib.mkOption {
      type = lib.types.bool;
      default = pkgs.stdenv.hostPlatform.isGhcjs;
      defaultText = lib.literalExpression ''
        pkgs.stdenv.hostPlatform.isGhcjs;
      '';
      example = true;
      description = lib.mdDoc ''
        When building to JS we need the patched versions of packages included in https://github.com/input-output-hk/hackage-overlay-ghcjs.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    repositories = [{
      name = "ghcjs-overlay";
      url = "https://raw.githubusercontent.com/input-output-hk/hackage-overlay-ghcjs/91f4ce9bea0e7f739b7495647c3f72a308ed1c6f";
      secure = true;
      sha256 = "sha256-mZT7c+xR5cUTjLdCqOxpprjYL3kr/+9rmumtXvWAQlM=";
      override = true;
    }];
  };
}
